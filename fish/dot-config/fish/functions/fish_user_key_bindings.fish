function fish_user_key_bindings
    bind --erase --preset \cv \cx
    bind \e\[122\;9u undo  # super+z
    bind \e\[122\;10u redo # super+shift+z

    # scroll the current screen contents into the scrollback buffer and clear the screen
    # bind \cl "seq -f '' (math $LINES - 1); clear | string replace \e\[3J ''; commandline -f repaint"
end
