#!/bin/bash

# Start ljmp
tmux send -t ljmp "./ljmp" ENTER
sleep 0.1
tmux send -t ljmp "かなた"
tmux send -t ljmp C-x
tmux capture-pane -p | head -n 2 | diff -E -u test_cut.ok -
if [ $? -ne 0 ]
then
   # 変化があり
   echo "Test_cut Failed."
fi
#tmux send -t ljmp "はるか"
#tmux display -p '#{cursor_x}, #{cursor_y}'

# Backspace の代わり
tmux send -t ljmp
tmux send -t ljmp C-V
tmux capture-pane -p | head -n 2 | diff -E -u test_cut2.ok -
if [ $? -ne 0 ]
then
   # 変化があり
   echo "Test_cut Failed."
fi
tmux send -t ljmp C-q C-q C-q
