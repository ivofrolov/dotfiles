function edit -d "Edit file in Emacs"
    emacsclient -q -n -a "emacs" $argv &; disown
end
