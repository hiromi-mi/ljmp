#!/bin/bash

# tmux is required.

# Start Session
tmux -u new-session -d -x 90 -y 10 -t ljmp

source ./test1.sh
source ./test_cut.sh
source ./test_syntax.sh
source ./test_undo.sh
source ./test_find.sh

# Start ljmp
#tmux send -t ljmp "./ljmp ljmp.c" ENTER
#sleep 0.1
#tmux capture-pane -p
# Capture With Colour
#tmux capture-pane -p -e

# Getting Cursor Position
#tmux display -p '#{cursor_x}'
#tmux display -p '#{cursor_y}'

# Quit ljmp
#tmux send -t ljmp C-q C-q C-q
tmux -u kill-session -t ljmp

