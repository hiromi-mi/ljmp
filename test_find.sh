# Start ljmp
tmux send -t ljmp "./ljmp test_find.c" ENTER
sleep 0.1
tmux capture-pane -p -e > /tmp/ljmp_test_find_result.ok

tmux send -t ljmp C-f inclu
tmux capture-pane -p -e
tmux send -t ljmp [ HOME
tmux capture-pane -p -e | diff -u - /tmp/ljmp_test_find_result.ok
# Getting Cursor Position
#tmux display -p '#{cursor_x}'
#tmux display -p '#{cursor_y}'

# Quit ljmp
tmux send -t ljmp C-q C-q C-q
sleep 0.1
tmux -u kill-session -t ljmp

