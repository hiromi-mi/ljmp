CC = clang
CFLAGS = -g

ljmp: ljmp.c
	$(CC) $(CFLAGS) ljmp.c -o ljmp -Wall -Wextra -Wpedantic -std=c17

clean:
	rm -f ljmp

