#!/bin/bash

# Start ljmp
tmux send -t ljmp "./ljmp test_syntax.c" ENTER
sleep 0.1
tmux capture-pane -p -e | head -n 7 | diff -E -u test_syntax.ok -
if [ $? -ne 0 ]
then
   # 変化があり
   echo "Test_syntax Failed."
fi
tmux send -t ljmp C-q C-q C-q
