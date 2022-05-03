if status is-interactive
    if command -s bat > /dev/null 2>&1
        alias cat bat
    end

    if command -s trash > /dev/null 2>&1
        alias rm trash
    end

    if string match -eq 'kitty' -- $TERM
        alias ssh "kitty +kitten ssh"
    end
end
