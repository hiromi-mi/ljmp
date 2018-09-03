#!/bin/bash

# Testsuite no.1 ライセンス表記の確認

# Start ljmp
tmux send -t ljmp "./ljmp test1.ok" ENTER
sleep 0.1
tmux capture-pane -p | head -n 8 | diff -u test1.ok -
if [ $? -ne 0 ]
then
   # 変化があり
   echo "Test1 Failed."
fi
tmux send -t ljmp C-q C-q C-q

# Start ljmp with Open Command
tmux send -t ljmp "./ljmp" ENTER
sleep 0.1
tmux send -t ljmp C-o "test1.ok" ENTER
tmux capture-pane -p | head -n 8 | diff -u test1.ok -
if [ $? -ne 0 ]
then
   # 変化があり
   echo "Test1 Failed."
fi
tmux send -t ljmp C-q C-q C-q
