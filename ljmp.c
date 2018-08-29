/* ljmp text editor
 *
 * appreciate to kilo text editor:
 *    https://viewsourcecode.org/snaptoken/kilo/
 *    https://github.com/antirez/kilo
 *
 * License: BSD 2-clause License
 * (Begin of License)
 *
 * Copyright (c) 2016, Salvatore Sanfilippo <antirez at gmail dot com>
 * (Extended by) Copyright (c) 2018, hiromi-mi <hiromi(hyphen)mi (at) cat (dot)
 * zaq (dot) jp>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * (End of License)
 */

/*** includes ***/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE // to use getline(): GNU Extension

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <locale.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>
#include <wchar.h>

/** defines ***/
#define LJMP_VERSION "0.0.2"
#define LJMP_TAB_STOP 8
#define LJMP_QUIT_TIMES 2

#define CTRL_KEY(k) ((k)&0x1f)
#define termsend(str, bytes) (write(STDOUT_FILENO, str, bytes))
#define termread(str, bytes) (read(STDIN_FILENO, str, bytes))

enum editorKey {
   BACKSPACE = 127,
   ARROW_LEFT = 1000,
   ARROW_RIGHT,
   ARROW_UP,
   ARROW_DOWN,
   DEL_KEY,
   HOME_KEY,
   END_KEY,
   PAGE_UP,
   PAGE_DOWN
};

enum editorHighlight {
   HL_NORMAL = 0,
   HL_COMMENT,
   HL_MLCOMMENT,
   HL_KEYWORD1,
   HL_KEYWORD2,
   HL_STRING,
   HL_NUMBER,
   HL_MATCH
};

#define HL_HIGHLIGHT_NUMBERS (1 << 0)
#define HL_HIGHLIGHT_STRINGS (1 << 1)

/*** prototypes ***/

void editorSetStatusMessage(const char *fmt, ...);
void editorRefreshScreen();
char *editorPrompt(char *prompt, void (*callback)(char *, int));
void editorUndoInit();
void abAppend(struct abuf *ab, const char *s, int len);
void abFree(struct abuf *ab);

// ほんとうに「バッファ」 write() 溜め
struct abuf {
   char *b;
   int len;
};

/*** data ***/

struct editorSyntax {
   char *filetype;   // ステータスバーの表示名
   char **filematch; // ファイル拡張子などのマッチパターン
   char **keywords;
   char *singleline_comment_start;
   char *multiline_comment_start;
   char *multiline_comment_end;
   int flags; // 何をハイライトさせるか. bit flag.
};

struct undoAPI {
   int old_cx;
   int new_cx;
   int cy;
   // struct abuf buf;
};

/* 行 */
typedef struct erow {
   int idx;      // ファイル内でのindex
   int bsize;    // バイナリ的な長さ
   int rsize;    // 実際に描画するテキストの長さ
   int csize;    // 文字数
   char *chars;  // 格納されたテキスト
   char *render; // 実際に描画するテキスト
   unsigned char *hl;
   int hl_open_comment;
   int indentations; // インデント行数
} erow;

struct editorConfig {
   int cx, cy;     // current position within the file
   int rx;         /* render の進み具合 */
   int bx;         /* バイナリ列としての文字位置 */
   int rowoff;     // row offset: スクロールの天井にある位置
   int coloff;     // col offset: スクロールの左端にある位置
   int screenrows; // 端末(ry
   int screencols;
   int numrows; // 行数
   erow *row;   // 実際の内容
   int dirty;
   char *filename;
   char statusmsg[80];
   time_t statusmsg_time;
   struct editorSyntax *syntax;
   struct termios orig_termios;
   struct undoAPI undo;
   struct abuf *copybuf;
};

struct editorConfig E;

/*** filetypes ***/

char *C_HL_extensions[] = {".c", ".h", ".cpp", NULL};
char *C_HL_keywords[] = {"switch", "if", "while", "for", "break", "continue",
                         "return", "else", "struct", "union", "typedef",
                         "static", "enum", "class", "case",

                         // 型 (KEYWORD2)
                         "int|", "long|", "double|", "float|", "char|",
                         "unsigned|", "signed|", "void|", NULL};

struct editorSyntax HLDB[] = {
    {"c", C_HL_extensions, C_HL_keywords, "//", "/*", "*/",
     HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS},
};

// エントリ数
#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

/*** unicode support ***/
/*** terminal ***/

void die(const char *s) {
   // 最低限戻して終了させる
   termsend("\x1b[2J", 4);
   termsend("\x1b[H", 3);
   perror(s);
   exit(1);
}

void disableRawMode() {
   // orig_termios への復旧を試みる
   if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1) {
      die("tcsetattr");
   }
}

void enableRawMode() {
   struct termios raw;

   if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1)
      die("tcgetattr");
   atexit(disableRawMode);

   raw = E.orig_termios;
   raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
   raw.c_oflag &= ~(OPOST);
   raw.c_cflag |= (CS8);
   raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
   raw.c_cc[VMIN] = 0;
   raw.c_cc[VTIME] = 1;

   if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1)
      die("tcsetattr");
}

int editorReadKey() {
   int nread;
   char c;
   while ((nread = termread(&c, 1)) != 1) {
      if (nread == -1 && errno != EAGAIN)
         die("read");
   }

   if (c == '\x1b') {
      char seq[3];

      if (termread(&seq[0], 1) != 1)
         return '\x1b';
      if (termread(&seq[1], 1) != 1)
         return '\x1b';

      if (seq[0] == '[') {
         if (seq[1] >= '0' && seq[1] <= '9') {
            if (termread(&seq[2], 1) != 1)
               return '\x1b';
            if (seq[2] == '~') {
               switch (seq[1]) {
               case '1':
                  return HOME_KEY;
               case '3':
                  return DEL_KEY;
               case '4':
                  return END_KEY;
               case '5':
                  return PAGE_UP;
               case '6':
                  return PAGE_DOWN;
               case '7':
                  return HOME_KEY;
               case '8':
                  return END_KEY;
               }
            }
         } else {
            switch (seq[1]) {
            case 'A':
               return ARROW_UP;
            case 'B':
               return ARROW_DOWN;
            case 'C':
               return ARROW_RIGHT;
            case 'D':
               return ARROW_LEFT;
            case 'H':
               return HOME_KEY;
            case 'F':
               return END_KEY;
            }
         }
      } else if (seq[0] == '0') {
         switch (seq[1]) {
         case 'H':
            return HOME_KEY;
         case 'F':
            return END_KEY;
         }
      }
      return '\x1b';
   } else {
      return c;
   }
}

int getCursorPosition(int *rows, int *cols) {
   char buf[32];
   unsigned int i = 0;

   if (termsend("\x1b[6n", 4) != 4)
      return -1;

   while (i < sizeof(buf) - 1) {
      if (termread(&buf[i], 1) != 1)
         break;
      if (buf[i] == 'R')
         break;
      i++;
   }
   buf[i] = '\0';

   if (buf[0] != '\x1b' || buf[1] != '[')
      return -1;
   if (sscanf(&buf[2], "%d;%d", rows, cols) != 2)
      return -1;

   editorReadKey();
   return -1;
}

int getWindowSize(int *rows, int *cols) {
   struct winsize ws;

   if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
      if (termsend("\x1b[999C\x1b[999B", 12) != 12)
         return -1;
      return getCursorPosition(rows, cols);
   } else {
      *cols = ws.ws_col;
      *rows = ws.ws_row;
      return 0;
   }
}

/*** syntax highlighting ***/

// 区切りの概念を識別
int is_separator(int c) {
   // strchr で出現位置を表せる
   return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void editorUpdateSyntax(erow *row) {
   row->hl = realloc(row->hl, row->rsize);
   // 何もなければHL_NORMAL 扱い
   memset(row->hl, HL_NORMAL, row->rsize);

   if (E.syntax == NULL)
      return;

   char **keywords = E.syntax->keywords;

   char *scs = E.syntax->singleline_comment_start;
   char *mcs = E.syntax->multiline_comment_start;
   char *mce = E.syntax->multiline_comment_end;

   int scs_len = scs ? strlen(scs) : 0;
   int mcs_len = mcs ? strlen(mcs) : 0;
   int mce_len = mce ? strlen(mce) : 0;

   int prev_sep = 1;  // 前の文字が区切りか否か
   int in_string = 0; // 文字列内にあるか否か
   int in_comment = (row->idx > 0 && E.row[row->idx - 1].hl_open_comment);

   int i = 0;
   while (i < row->rsize) {
      char c = row->render[i];
      unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

      if (scs_len && !in_string && !in_comment) {
         // 最初のn byte しか比較しない. // かどうか.
         if (!strncmp(&row->render[i], scs, scs_len)) {
            memset(&row->hl[i], HL_COMMENT, row->rsize - i);
            break;
         }
      }

      if (mcs_len && mce_len && !in_string) {
         if (in_comment) {
            row->hl[i] = HL_MLCOMMENT;
            /* コメント終端を判定 */
            if (!strncmp(&row->render[i], mce, mce_len)) {
               memset(&row->hl[i], HL_COMMENT, mce_len);
               i += mce_len;
               in_comment = 0;
               prev_sep = 1;
               continue;
            } else {
               i++;
               continue;
            }
         } else if (!strncmp(&row->render[i], mcs, mcs_len)) {
            /* コメント起点を反転 */
            memset(&row->hl[i], HL_MLCOMMENT, mcs_len);
            i += mcs_len;
            in_comment = 1;
            continue;
         }
      }

      if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
         if (in_string) {
            row->hl[i] = HL_STRING;
            if (c == '\\' && i + 1 < row->rsize) {
               row->hl[i + 1] = HL_STRING;
               // \ の直後の文字は気にしない
               i += 2;
               continue;
            }
            if (c == in_string)
               in_string = 0; // closing quote なら閉じる
            i++;
            prev_sep = 1; // 最後に閉じ括弧が区切り扱いされるように
            continue;
         } else {
            if (c == '"' || c == '\'') {
               // 始まるから開始する
               in_string = c;
               row->hl[i] = HL_STRING;
               i++;
               continue;
            }
         }
      }

      // 前の文字の種類
      if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) { // 数値を強調するかどうか
         // 区切られた後の数値
         if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
             (c == '.' && prev_hl == HL_NUMBER)) { // 小数
            row->hl[i] = HL_NUMBER;
            i++;
            prev_sep = 0;
            continue;
         }
      }

      // 前が区切り文字でなければ、avoid などが問題となる
      if (prev_sep) {
         int j;
         for (j = 0; keywords[j]; j++) { /* 最後はNULL だから問題ない */
            int klen = strlen(keywords[j]);
            /* 2種類目のキーワードには | がついているから */
            int kw2 = keywords[j][klen - 1] == '|';
            if (kw2)
               klen--;

            // \0 が区切り文字として認識されるから
            if (!strncmp(&row->render[i], keywords[j], klen) &&
                is_separator(row->render[i + klen])) {
               memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
               i += klen;
               break;
            }
         }
         if (keywords[j] != NULL) {
            prev_sep = 0; // 区切り
            continue;
         }
      }

      /* 残り */
      prev_sep = is_separator(c);
      i++;
   }

   int changed = (row->hl_open_comment != in_comment);
   // hl_open_comment の状況は順伝搬してゆく
   row->hl_open_comment = in_comment;
   // コメント行扱い. コメント行かどうかが変わったときだけ
   if (changed && row->idx + 1 < E.numrows)
      editorUpdateSyntax(&E.row[row->idx + 1]);
}

int editorSyntaxToColor(int hl) {
   switch (hl) {
   case HL_COMMENT:
   case HL_MLCOMMENT:
      return 36; // cyan
   case HL_KEYWORD1:
      return 33; // yellow
   case HL_KEYWORD2:
      return 32; // green
   case HL_STRING:
      return 35; // magenta
   case HL_NUMBER:
      return 31; // red
   case HL_MATCH:
      return 34; // blue
   default:
      return 37; // white
   }
}

void editorSelectSyntaxHighlight() {
   E.syntax = NULL;
   if (E.filename == NULL)
      return;

   // 右端にある '.' の位置を探す
   char *ext = strrchr(E.filename, '.');

   for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
      struct editorSyntax *s = &HLDB[j];
      unsigned int i = 0;
      // 最後はNULL になるという規約に基き
      while (s->filematch[i]) {
         int is_ext = (s->filematch[i][0] == '.');
         // 拡張子による判定
         if ((is_ext && ext && !strcmp(ext, s->filematch[i])) ||
             // 単に含まれているかの判定
             (!is_ext && strstr(E.filename, s->filematch[i]))) {
            E.syntax = s;

            /* 改変後に Syntax を丸ごと計算し直す */
            int filerow;
            for (filerow = 0; filerow < E.numrows; filerow++) {
               editorUpdateSyntax(&E.row[filerow]);
            }
            return;
         }
         i++;
      }
   }
}

/*** row operations ***/

int editorRowCxToBxRx(erow *row, const int cx, int *save_bx, int *save_rx) {
   int bx = 0, rx = 0, cur_cx;
   int width;
   wchar_t chr;
   for (cur_cx = 0; cur_cx < cx; cur_cx++) {
      if (row->chars[bx] == '\t') {
         // タブ文字
         rx += (LJMP_TAB_STOP) - (rx % LJMP_TAB_STOP);
         bx += 1;
         continue;
      }

      // コードポイントのバイト数 see utf-8 (7)
      if ((row->chars[bx] & 0x80) == 0) { // 0xxxxxxx
         chr = row->chars[bx];
         bx += 1;
      } else if ((row->chars[bx] & 0xE0) == 0xC0) {
         // 110xxxxx
         chr = ((row->chars[bx] & 0x1F) << 6) + (row->chars[bx + 1] & 0x3F);
         bx += 2;
      } else if ((row->chars[bx] & 0xF0) == 0xE0) {
         // 1110xxxx
         chr = ((row->chars[bx] & 0x0F) << 12) +
               ((row->chars[bx + 1] & 0x3F) << 6) + (row->chars[bx + 2] & 0x3F);
         bx += 3;
      } else if ((row->chars[bx] & 0xF8) == 0xF0) {
         // 11110xxx
         chr = ((row->chars[bx] & 0x07) << 18) +
               ((row->chars[bx + 1] & 0x3F) << 12) +
               ((row->chars[bx + 2] & 0x3F) << 6) + (row->chars[bx + 3]);
         bx += 4;
      } else {
         // Incorrect Sequence. Treat as Binary.
         bx += 1;
      }
      // https://www.unicode.org/Public/UCD/latest/ucd/EastAsianWidth.txt
      // これが本来必要かもしれない
      width = wcwidth(chr);
      if (width >= 0) {
         rx += width;
      }
      // width < 0 when unprintable chars
      if (bx >= row->bsize) {
         break;
      }
   }
   if (save_bx != NULL) {
      *save_bx = bx;
   }
   if (save_rx != NULL) {
      *save_rx = rx;
   }
   return bx;
}

int editorRowBxToCx(erow *row, int bx) {
   int cur_cx = 0;
   int cur_bx = 0;
   int width;
   wchar_t chr;
   int rx = 0;
   for (cur_cx = 0;; cur_cx++) {
      if (row->chars[cur_bx] == '\t') {
         // タブ文字
         rx += (LJMP_TAB_STOP) - (rx % LJMP_TAB_STOP);
         cur_bx += 1;
         continue;
      }

      // コードポイントのバイト数 see utf-8 (7)
      if ((row->chars[cur_bx] & 0x80) == 0) { // 0xxxxxxx
         chr = row->chars[cur_bx];
         cur_bx += 1;
      } else if ((row->chars[cur_bx] & 0xE0) == 0xC0) {
         // 110xxxxx
         chr = ((row->chars[cur_bx] & 0x1F) << 6) +
               (row->chars[cur_bx + 1] & 0x3F);
         cur_bx += 2;
      } else if ((row->chars[cur_bx] & 0xF0) == 0xE0) {
         // 1110xxxx
         chr = ((row->chars[cur_bx] & 0x0F) << 12) +
               ((row->chars[cur_bx + 1] & 0x3F) << 6) +
               (row->chars[cur_bx + 2] & 0x3F);
         cur_bx += 3;
      } else if ((row->chars[cur_bx] & 0xF8) == 0xF0) {
         // 11110xxx
         chr = ((row->chars[cur_bx] & 0x07) << 18) +
               ((row->chars[cur_bx + 1] & 0x3F) << 12) +
               ((row->chars[cur_bx + 2] & 0x3F) << 6) +
               (row->chars[cur_bx + 3]);
         cur_bx += 4;
      } else {
         // Incorrect Sequence. Treat as Binary.
         cur_bx += 1;
      }
      // https://www.unicode.org/Public/UCD/latest/ucd/EastAsianWidth.txt
      // これが本来必要かもしれない
      width = wcwidth(chr);
      if (width >= 0) {
         rx += width;
      }
      // width < 0 when unprintable chars
      if (cur_bx > bx) {
         break;
      }
   }
   return cur_cx;
}

// editorRowCxToRx の逆演算
int editorRowRxToCx(erow *row, int rx) {
   int cur_rx = 0;
   int cx;
   int width;
   wchar_t chr;
   for (cx = 0; cx < row->bsize; cx++) {
      /* cx から rx への変換として加えていき...*/
      if (row->chars[cx] == '\t') {
         // タブ文字
         // ここの -1 は rx++ があるから
         cur_rx += (LJMP_TAB_STOP - 1) - (cur_rx % LJMP_TAB_STOP);
      }
      cur_rx++;

      // UTF-8 への対応
      // コードポイントのバイト数 see utf-8 (7)
      if ((row->chars[cx] & 0x80) == 0) { // 0xxxxxxx
         chr = row->chars[cx];
      } else if ((row->chars[cx] & 0xE0) == 0xC0) {
         // 110xxxxx
         chr = ((row->chars[cx] & 0x1F) << 6) + (row->chars[cx + 1] & 0x3F);
         cx += 1;
      } else if ((row->chars[cx] & 0xF0) == 0xE0) {
         // 1110xxxx
         chr = ((row->chars[cx] & 0x0F) << 12) +
               ((row->chars[cx + 1] & 0x3F) << 6) + (row->chars[cx + 2] & 0x3F);
         cx += 2;
      } else if ((row->chars[cx] & 0xF8) == 0xF0) {
         // 11110xxx
         chr = ((row->chars[cx] & 0x07) << 18) +
               ((row->chars[cx + 1] & 0x3F) << 12) +
               ((row->chars[cx + 2] & 0x3F) << 6) + (row->chars[cx + 3]);
         cx += 3;
      } else {
         // 不正なシーケンス
         puts("Incorrect Sequence.");
         exit(-2);
      }
      width = wcwidth(chr);
      if (width >= 0) {
         cur_rx += width;
      }
      // 必ずここでひろわれるはず: 超えたら返す
      if (cur_rx > rx)
         return cx;
   }
   // ここの行にはたどりつかないはず
   return cx;
}

// rx が多バイト文字の途中といった中途半端な場所であるとき, 左側に寄せる
int editorRowRxBisectLeft(erow *row, int rx) {
   const int j = (1 << 8) - 1;
   const int k = j & ~(1 << 6);
   for (int i = rx; i > rx - 3; i--) {
      // つまり、文字の1バイト目か否か
      if (i < 0 || ((j | row->render[i]) == k)) {
         return i;
      }
   }
   return rx;
}
// rx が多バイト文字の途中といった中途半端な場所であるとき, 右側に寄せる
int editorRowRxBisectRight(erow *row, int rx) {
   const int j = (1 << 8) - 1;
   const int k = j & ~(1 << 6);
   for (int i = rx; i < rx + 3; i++) {
      // つまり、文字の1バイト目か否か
      if (i >= row->rsize || ((j | row->render[i]) == k)) {
         return i;
      }
   }
   return rx;
}

// render の内容を埋める
void editorUpdateRow(erow *row) {
   int tabs = 0;
   int j;
   row->indentations = 0;
   for (j = 0; j < row->bsize; j++) {
      // タブの数を数える
      if (row->chars[j] == '\t') {
         if (j == row->indentations) {
            row->indentations++;
         }
         tabs++;
      }
   }
   free(row->render);
   // 8 - 1 = 7 (1 分はすでにrow->bsize として確保してある)
   row->render = malloc(row->bsize + tabs * (LJMP_TAB_STOP - 1) + 1);

   int idx = 0;
   // 実体から表示に割り当てる
   for (j = 0; j < row->bsize; j++) {
      if (row->chars[j] == '\t') {
         // タブが来たら" "で埋める. 丁度いい数になるまで合わせる
         row->render[idx++] = ' ';
         while (idx % LJMP_TAB_STOP != 0)
            row->render[idx++] = ' ';
      } else {
         row->render[idx++] = row->chars[j];
      }
   }
   row->render[idx] = '\0';
   row->rsize = idx;
   row->csize = editorRowBxToCx(row, row->bsize);

   editorRowCxToBxRx(&E.row[E.cy], E.cx, &(E.bx), &(E.rx));
   editorUpdateSyntax(row);
}

// 行の追記
void editorInsertRow(int at, char *s, size_t len) {
   if (at < 0 || at > E.numrows)
      return;

   E.row = realloc(E.row, sizeof(erow) * (E.numrows + 1));
   memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numrows - at));
   // その後の行について index を改める
   for (int j = at + 1; j <= E.numrows; j++)
      E.row[j].idx++;

   E.row[at].idx = at;
   E.row[at].indentations = 0;
   E.row[at].bsize = len;
   E.row[at].chars = malloc(len + 1);
   memcpy(E.row[at].chars, s, len);
   E.row[at].chars[len] = '\0';

   E.row[at].rsize = 0;
   E.row[at].csize = 0;
   E.row[at].render = NULL;
   E.row[at].hl = NULL;
   E.row[at].hl_open_comment = 0;
   E.numrows++;
   editorUpdateRow(&E.row[at]);
   E.dirty++; // こうしておくことで、改変具合が分かって便利
}

void editorFreeRow(erow *row) {
   // row を削除
   free(row->render);
   free(row->chars);
   free(row->hl);
}

void editorDelRow(int at) {
   // at にある行を削除
   if (at < 0 || at >= E.numrows)
      return;
   editorFreeRow(&E.row[at]);
   memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
   for (int j = at; j < E.numrows; j++)
      E.row[j].idx--;
   E.numrows--;
   E.dirty++;
}

void editorRowAppendString(erow *row, char *s, size_t len) {
   row->chars = realloc(row->chars, row->bsize + len + 1);
   // row->bsize 以降に文字列をつけ加える
   memcpy(&row->chars[row->bsize], s, len);
   row->bsize += len;
   row->chars[row->bsize] = '\0';
   editorUpdateRow(row);
   E.dirty++;
}

void editorRowInsertChar(erow *row, int at, char c) {
   // 文字を挿入するindex
   if (at < 0 || at > row->bsize)
      at = row->bsize;
   row->chars = realloc(row->chars, row->bsize + 2); // 端っこ及び隣
   // memcpy とは違い被ってても大丈夫らしい
   memmove(&row->chars[at + 1], &row->chars[at], row->bsize - at);
   row->bsize++;
   row->chars[at] = c;
   editorUpdateRow(row);
   E.dirty++;
}

void editorRowDelChar(erow *row, int at) {
   // 1文字消す
   if (at < 0 || at > row->bsize)
      return;
   // 文字列からバイナリへの変換
   int bx_before = editorRowCxToBxRx(row, at, NULL, NULL);
   int bx_after = editorRowCxToBxRx(row, at + 1, NULL, NULL);
   if (bx_before != bx_after) {
      memmove(&row->chars[bx_before], &row->chars[bx_after], row->bsize - at);
      row->bsize -= (bx_after - bx_before);
      editorUpdateRow(row);
      E.dirty++;
   }
}

/*** editor operations ***/

// undo されます、行移動します. 好きにやるには、 editorRowInsertChar() を利用
void editorInsertChar(int c) {
   if (E.cy == E.numrows) {
      // 行を追記
      editorInsertRow(E.numrows, "", 0);
   }
   editorRowInsertChar(&E.row[E.cy], E.bx, c);
   E.cx++;
   // undo 対象に追加
   if (E.undo.cy != E.cy) {
      E.undo.cy = E.cy;
      E.undo.old_cx = E.cx - 1;
      E.undo.new_cx = E.cx;
   } else {
      E.undo.new_cx += 1;
   }
}

void editorInsertNewline() {
   // 改行した後の初期位置. インデントとかコメントを調整すると増える
   int first_cx = 0;
   if (E.cx == 0) {
      editorInsertRow(E.cy, "", 0);
   } else {
      erow *row = &E.row[E.cy];
      // E.cx 以降の内容を次の行に挿入
      editorInsertRow(E.cy + 1, &row->chars[E.bx], row->bsize - E.bx);
      // editorInsertRow によって realloc が呼ばれポインタの位置が移動した
      row = &E.row[E.cy];

      // Automatic Indentation
      // E.bx-1 としても、 E.cx > 0 より大丈夫
      if (E.row[E.cy].chars[E.bx - 1] == '{') {
         E.row[E.cy + 1].indentations = E.row[E.cy].indentations + 1;
      } else {
         E.row[E.cy + 1].indentations = E.row[E.cy].indentations;
      }

      for (int i = 0; i < E.row[E.cy + 1].indentations; i++) {
         editorRowInsertChar(&E.row[E.cy + 1], 0, '\t');
      }
      first_cx = E.row[E.cy + 1].indentations;

      // Automatic Comment-out
      if (E.row[E.cy].chars[E.row[E.cy].indentations] == '/') {
         // 複数行コメントに対応していない
         editorRowInsertChar(&E.row[E.cy + 1], E.row[E.cy + 1].indentations,
                             '/');
         editorRowInsertChar(&E.row[E.cy + 1], E.row[E.cy + 1].indentations + 1,
                             '/');
         editorRowInsertChar(&E.row[E.cy + 1], E.row[E.cy + 1].indentations + 2,
                             ' ');
         first_cx += 3;
      }
      row->bsize = E.bx;
      row->chars[row->bsize] = '\0';
      // 更新
      editorUpdateRow(row);
   }
   // 次の行に移る
   E.cy++;
   E.cx = first_cx;
}

void editorDelChar() {
   // 消すものがないじょうきょう ではそのまま
   if (E.cy == E.numrows)
      return;
   // 最初の最初ならそのまま
   if (E.cx == 0 && E.cy == 0)
      return;

   erow *row = &E.row[E.cy];
   if (E.cx > 0) {
      editorRowDelChar(row, E.cx - 1);
      E.cx--;
   } else {
      E.cx = E.row[E.cy - 1].csize;
      // 前の行に残りを残して
      editorRowAppendString(&E.row[E.cy - 1], row->chars, row->bsize);
      // その行を消す
      editorDelRow(E.cy);
      E.cy--;
   }
}

/*** file i/o ***/

char *editorRowToString(int *buflen) {
   /* Expecting the caller to free() the memory */
   int totlen = 0;
   int j;
   for (j = 0; j < E.numrows; j++) {
      totlen += E.row[j].bsize + 1;
   }
   // 長さを知らせる
   *buflen = totlen;

   char *buf = malloc(totlen);
   char *p = buf; // 全バッファを繋げる
   for (j = 0; j < E.numrows; j++) {
      memcpy(p, E.row[j].chars, E.row[j].bsize);
      p += E.row[j].bsize;
      *p = '\n';
      p++;
   }

   return buf;
}

void editorOpen(char *filename) {
   free(E.filename);
   E.filename = strdup(filename); // 文字列の複製

   editorSelectSyntaxHighlight();
   FILE *fp = fopen(filename, "r");
   if (!fp)
      die("fopen");

   char *line = NULL; // こうすると getline() 側で勝手にやってくれる
   size_t linecap = 0;
   ssize_t linelen;
   while ((linelen = getline(&line, &linecap, fp)) != -1) {
      while (linelen > 0 &&
             (line[linelen - 1] == '\n' || line[linelen - 1] == '\r'))
         linelen--;
      editorInsertRow(E.numrows, line, linelen);
   }
   free(line); // getline() が失敗したとしても free() が必要
   fclose(fp);
   E.dirty = 0;
}

void editorSave() {
   if (E.filename == NULL) {
      E.filename = editorPrompt("Save as: %s", NULL);
      if (E.filename == NULL) {
         editorSetStatusMessage("Save aborted");
         return;
      }
      editorSelectSyntaxHighlight();
   }

   int len;
   char *buf = editorRowToString(&len);
   // O_CREAT: 存在しなければ新規作成, O_RDWR: 両方
   // 0644: Permission
   // open() で O_TRUNC を渡して、write() が失敗すると無理かも
   int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
   if (fd != -1) {
      // ファイルの長さを削る
      if (ftruncate(fd, len) != -1) {
         if (write(fd, buf, len) == len) {
            close(fd);
            free(buf);
            editorSetStatusMessage("%d bytes written on disk", len);
            E.dirty = 0;
            return;
         }
      }
      ftruncate(fd, len);
      write(fd, buf, len);
      close(fd);
   }
   free(buf);
   editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));

   // undo を無効にする
   editorUndoInit(E.undo);
}

/*** find ***/

void editorFindCallBack(char *query, int key) {
   static int last_match = -1; // last match における row
   static int direction = 1;   // 1 : forward, -1 : backward

   static int saved_hl_line; // 保存されたハイライト
   // 問題点: 複数行マッチできない
   static char *saved_hl = NULL; // 復元用の強調状況

   // 保存してから復元するまでの間にファイルを編集できないから問題なく動ける
   if (saved_hl) {
      // 復元
      memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);
      free(saved_hl);
      saved_hl = NULL;
   }
   if (key == '\r' || key == '\x1b') {
      // 新規検索の開始
      last_match = -1;
      direction = 1; // 後ろのif があるしこの行は不要?
      return;
   } else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
      direction = 1;
   } else if (key == ARROW_LEFT || key == ARROW_UP) {
      direction = -1;
   } else {
      // 関係ないことされたらリセット
      last_match = -1;
      direction = 1; // この行は不要?
   }

   if (last_match == -1)
      direction = 1;
   int current = last_match;
   int i; // もはやi は一周検索したかの判定にしか用いられない
   for (i = 0; i < E.numrows; i++) {
      current += direction; // 次or前の行
      if (current == -1) {
         current = E.numrows - 1; // 最後までくるり
      } else if (current == E.numrows) {
         current = 0; // 最初へもどる
      }

      erow *row = &E.row[current];
      char *match = strstr(row->render, query);
      // 存在しなければ null
      if (match) {
         last_match = current;
         E.cy = current;
         E.cx = editorRowRxToCx(row, match - row->render); // ポインタ演算
         E.rowoff = E.numrows; // あえて一番下の行に配置することにして、
         // editorScroll() を呼び、目的行に行く

         // match - row->render がそのマッチ位置, そのうちquery文字塗る
         saved_hl_line = current;
         saved_hl = malloc(row->rsize); // その行を保存
         memcpy(saved_hl, row->hl, row->rsize);
         memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
         break;
      }
   }
}

void editorFind() {
   int saved_cx = E.cx;
   int saved_cy = E.cy;
   int saved_rowoff = E.rowoff;
   int saved_coloff = E.coloff;

   char *query = editorPrompt("Search: %s (ESC to cancel, Arrows/Enter)",
                              editorFindCallBack);
   if (query) {
      free(query);
   } else {
      E.cx = saved_cx;
      E.cy = saved_cy;
      E.rowoff = saved_rowoff;
      E.coloff = saved_coloff;
   }
}

/*** undo ***/

void editorUndo() {
   if (E.undo.cy >= 0) { // undo できる
      for (int i = E.undo.new_cx - 1; i >= E.undo.old_cx; i--) {
         editorRowDelChar(&E.row[E.undo.cy], i);
      }
      if (E.cy == E.undo.cy) {
         // 同じ行にいるなら、元の位置に戻す
         E.cx = E.undo.old_cx;
      }
      editorUndoInit();
   }
}

void editorUndoInit() {
   E.undo.cy = -1;
   E.undo.old_cx = 0;
   E.undo.new_cx = 0;
}

/*** copy and paste ***/

void editorCopy() {
   struct abuf copybuf = ABUF_INIT;
   abAppend(&copybuf, E.row[E.cy].chars, E.row[E.cy].bsize);
   E.copybuf = copybuf;
   editorSetStatusMessage("Copy completed.");
}

void editorPaste() {
   if (E.copybuf != NULL) {
      abFree(E.copybuf);
      E.copybuf = NULL;
   }
}

/*** append buffer ***/

#define ABUF_INIT                                                              \
   { NULL, 0 }

void abAppend(struct abuf *ab, const char *s, int len) {
   char *new = realloc(ab->b, ab->len + len);
   if (new == NULL)
      return;
   memcpy(&new[ab->len], s, len);
   ab->b = new;
   ab->len += len;
}

void abFree(struct abuf *ab) { free(ab->b); }

/*** output ***/
// row offset と現状の帳尻を合わせる
void editorScroll() {
   E.rx = 0;
   if (E.cy < E.numrows) {
      editorRowCxToBxRx(&E.row[E.cy], E.cx, &(E.bx), &(E.rx));
   }

   /* ウインドウの上 */
   if (E.cy < E.rowoff) {
      E.rowoff = E.cy;
   }
   /* 下 */
   if (E.cy >= E.rowoff + E.screenrows) {
      E.rowoff = E.cy - E.screenrows + 1;
   }
   // 左端
   if (E.rx < E.coloff) {
      E.coloff = E.rx;
   }
   if (E.rx >= E.coloff + E.screencols) {
      E.coloff = E.rx - E.screencols + 1;
   }
}

void editorDrawRows(struct abuf *ab) {
   for (int y = 0; y < E.screenrows; y++) {
      int filerow = y + E.rowoff;
      if (filerow >= E.numrows) {
         if (E.numrows == 0 && y == E.screenrows / 3) {
            char welcome[80];
            // Welcome Message を表示
            int welcomelen =
                snprintf(welcome, sizeof(welcome), "Ljmp editor -- version %s",
                         LJMP_VERSION);
            if (welcomelen > E.screencols)
               welcomelen = E.screencols;
            // To centerlise
            int padding = (E.screencols - welcomelen) / 2;
            if (padding) {
               abAppend(ab, "~", 1);
               padding--;
            }
            while (padding--)
               abAppend(ab, " ", 1);
            abAppend(ab, welcome, welcomelen);
         } else {
            abAppend(ab, "~", 1);
         }
      } else {
         // ずらす
         int len = E.row[filerow].rsize - E.coloff;
         if (len > E.screencols)
            len = E.screencols;
         len = editorRowRxBisectRight(&E.row[filerow], len);

         // ポインタにしてやることでそのcoloff 以降全体を指すようになる
         // 略記法
         char *c = &E.row[filerow].render[E.coloff];
         unsigned char *hl = &E.row[filerow].hl[E.coloff];

         // 現在の色. -1 なら同じ、あとは editorSyntaxToColor の結果
         // 次の行に行ってもそのcolor の呼び出しが継続されるように
         static int current_color = -1;
         int j;
         for (j = 0; j < len; j++) {
            if (iscntrl(c[j])) {
               // アルファベットの大文字は @ のあとに続く
               char sym = (c[j] <= 26) ? '@' + c[j] : '?';
               abAppend(ab, "\x1b[7m", 4); // 反転色
               abAppend(ab, &sym, 1);
               abAppend(ab, "[\x1b[m", 3); // 完全に帳消しにする
               //強調は続く
               char buf[16];
               int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", current_color);
               abAppend(ab, buf, clen);
            } else if (hl[j] == HL_NORMAL) {
               if (current_color != -1) {
                  abAppend(ab, "\x1b[39m", 5);
                  current_color = -1;
               }
               abAppend(ab, &c[j], 1);
            } else {
               // 色と対応
               int color = editorSyntaxToColor(hl[j]);
               if (color != current_color) {
                  // 色合いが変化した
                  char buf[16];
                  // \x1b[m により色あいが8色定められる
                  int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
                  abAppend(ab, buf, clen);
                  current_color = color;
               }
               abAppend(ab, &c[j], 1);
               // 標準の色に戻す
            }
         }
         abAppend(ab, "\x1b[39m", 5);
         current_color = -1;
      }

      // Erase In Line (right of the cursor)
      abAppend(ab, "\x1b[K", 3);

      // 行をあける
      abAppend(ab, "\r\n", 2);
   }
}

void editorDrawStatusBar(struct abuf *ab) {
   abAppend(ab, "\x1b[7m", 4);                       // 色の反転
   char status[E.screencols], rstatus[E.screencols]; // 元々は80 だった
   int len = snprintf(status, sizeof(status), "%.20s - %d lines %s",
                      E.filename ? E.filename : "[No Name]", E.numrows,
                      E.dirty ? "(modified)" : "");
   int rlen =
       snprintf(rstatus, sizeof(rstatus), "%s | %d/%d",
                E.syntax ? E.syntax->filetype : "no ft", E.cy + 1, E.numrows);
   if (len > E.screencols)
      len = E.screencols; // 不要?
   abAppend(ab, status, len);
   while (len < E.screencols) {
      if (E.screencols - len == rlen) {
         abAppend(ab, rstatus, rlen);
         break;
      } else {
         abAppend(ab, " ", 1);
         len++;
      }
   }
   // [1;4;5;7m <- bold, underscore, blink, inverted
   abAppend(ab, "\x1b[m", 3); // clear attributes
   abAppend(ab, "\r\n", 2);
}

void editorDrawMessageBar(struct abuf *ab) {
   abAppend(ab, "\x1b[K", 3); // クリア
   int msglen = strlen(E.statusmsg);
   if (msglen > E.screencols)
      msglen = E.screencols;
   if (msglen && time(NULL) - E.statusmsg_time < 5) {
      // 最初の5秒間だけ書く
      abAppend(ab, E.statusmsg, msglen);
   }
}

void editorRefreshScreen() {
   editorScroll();

   struct abuf ab = ABUF_INIT;

   // h : turn on set mode
   // l : turn on reset mode
   // ?25 : VT100 以後に導入されたカーソルを隠す操作
   abAppend(&ab, "\x1b[?25l", 6);
   abAppend(&ab, "\x1b[2J", 4);
   abAppend(&ab, "\x1b[H", 3);

   editorDrawRows(&ab);
   editorDrawStatusBar(&ab);
   editorDrawMessageBar(&ab);

   char buf[32];
   // terminal: 1-indexed, Clang: 0-indexed
   snprintf(buf, sizeof(buf), "\x1b[%d;%dH", E.cy - E.rowoff + 1,
            E.rx - E.coloff + 1);
   abAppend(&ab, buf, strlen(buf));

   abAppend(&ab, "\x1b[?25h", 6);
   termsend(ab.b, ab.len);
   abFree(&ab);
}

void editorSetStatusMessage(const char *fmt, ...) {
   va_list ap;
   va_start(ap, fmt);
   vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
   va_end(ap);
   E.statusmsg_time = time(NULL);
}

/*** input ***/

char *editorPrompt(char *prompt, void (*callback)(char *, int)) {
   // user がbuf をfree() することが期待されている
   size_t bufsize = 128;
   char *buf = malloc(bufsize);

   size_t buflen = 0;
   // buf に保存される
   buf[0] = '\0';

   while (1) {
      editorSetStatusMessage(prompt, buf);
      editorRefreshScreen();

      int c = editorReadKey();
      if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
         // 文字列を減らす
         if (buflen != 0)
            buf[--buflen] = '\0';
      } else if (c == '\x1b') {
         // To cancel
         editorSetStatusMessage("");
         if (callback)
            callback(buf, c);
         free(buf);
         return NULL;
      } else if (c == '\r') {
         if (buflen != 0) {
            editorSetStatusMessage("");
            if (callback)
               callback(buf, c);
            return buf;
         }
         // 制御文字でないようにするため, 128 以下 ( editorKey の1000
         // 以上でない)
      } else if (!iscntrl(c) && c < 128) {
         // 溜められていく. 不足したら倍にして realloc
         if (buflen == bufsize - 1) {
            bufsize *= 2;
            buf = realloc(buf, bufsize);
         }
         // string にしておく
         buf[buflen++] = c;
         buf[buflen] = '\0';
      }
      if (callback)
         callback(buf, c);
   }
}

void editorMoveCursor(int key) {
   erow *row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
   switch (key) {
   case ARROW_LEFT:
      if (E.cx != 0) {
         E.cx--;
         editorSetStatusMessage("%d %d", E.cx, row->csize);
      } else if (E.cy > 0) {
         // 前の行の最後に移る
         E.cy--;
         E.cx = E.row[E.cy].csize;
      }
      break;
   case ARROW_RIGHT:
      // 横スクロール幅の制限
      if (row && E.cx < row->csize) {
         E.cx++;
         editorSetStatusMessage("%d %d", E.cx, row->csize);
      } else if (row && E.cx == row->csize) {
         E.cy++;
         E.cx = 0;
         editorSetStatusMessage("%d %d", E.cx, row->csize);
      }
      break;
   case ARROW_UP:
      if (E.cy != 0) {
         E.cy--;
      }
      break;
   case ARROW_DOWN:
      if (E.cy < E.numrows) {
         E.cy++;
      }
      break;
   }

   // 長い行から短い行へ移ったときの処理
   // 短い行の長さに戻す
   row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
   int rowlen = row ? row->csize : 0;
   if (E.cx > rowlen) {
      E.cx = rowlen;
   }
}

void editorProcessKeypress() {
   static int quit_times = LJMP_QUIT_TIMES;
   int c = editorReadKey();

   switch (c) {
   case '\r':
      editorInsertNewline();
      break;
   case CTRL_KEY('q'):
      if (E.dirty && quit_times > 0) {
         editorSetStatusMessage("WARNING!!! File has unsaved changes. "
                                "Press CTRL-Q %d more times to quit.",
                                quit_times);
         quit_times--;
         return;
      }
      termsend("\x1b[2J", 4);
      termsend("\x1b[H", 3);
      exit(0);
      break;

   case CTRL_KEY('s'):
      editorSave();
      break;

   case HOME_KEY:
      E.cx = 0;
      break;
   case END_KEY:
      if (E.cy < E.numrows) {
         E.cx = E.row[E.cy].rsize;
      }
      break;

   case CTRL_KEY('f'):
      editorFind();
      break;

   case BACKSPACE:
   case CTRL_KEY('h'):
   case DEL_KEY:
      if (c == DEL_KEY)
         editorMoveCursor(ARROW_RIGHT); // 右から消す
      editorDelChar();
      break;

   case CTRL_KEY('c'):
      editorCopy();
      break;
   case CTRL_KEY('v'):
      editorPaste();
      break;

   case CTRL_KEY('u'):
      editorUndo();
      break;

   case PAGE_UP:
   case PAGE_DOWN: {
      if (c == PAGE_UP) {
         E.cy = E.rowoff;
      } else if (c == PAGE_DOWN) {
         E.cy = E.rowoff + E.screenrows - 1;
         if (E.cy > E.numrows)
            E.cy = E.numrows;
      }
      int times = E.screenrows;
      while (times--)
         editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
   } break;

   case ARROW_UP:
   case ARROW_DOWN:
   case ARROW_LEFT:
   case ARROW_RIGHT:
      editorMoveCursor(c);
      break;
   case CTRL_KEY('l'):
   case '\x1b': // Esc
      break;
   default:
      editorInsertChar(c);
      break;
   }
}

/*** init ***/

void initEditor() {
   // "" とすると、環境変数が参照される
   setlocale(LC_ALL, "");
   E.cx = 0;
   E.cy = 0;
   E.rx = 0;
   E.bx = 0;
   E.numrows = 0; // 行数
   E.row = NULL;  // 行そのものを持つポインタ
   E.dirty = 0;
   E.rowoff = 0; // ファイルの先頭
   E.coloff = 0; // ファイルの先頭
   E.filename = NULL;
   E.statusmsg[0] = '\0';
   E.statusmsg_time = 0;
   E.syntax = NULL;
   E.copybuf = NULL;
   editorUndoInit();

   if (getWindowSize(&E.screenrows, &E.screencols) == -1)
      die("getWindowSize");
   E.screenrows -= 2; // For Status-Bar and Message
}

int main(int argc, char *argv[]) {
   enableRawMode();
   initEditor();
   if (argc >= 2) {
      editorOpen(argv[1]);
   }

   editorSetStatusMessage(
       "HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find | Ctrl-U = undo");

   while (1) {
      editorRefreshScreen();
      editorProcessKeypress();
   }
   return 0;
}
