if status is-interactive
    alias urlencode "string escape --style=url"
    alias urldecode "string unescape --style=url"

    if command -s bat > /dev/null 2>&1
        alias cat bat
    end

    if command -s trash > /dev/null 2>&1
        alias rm trash
    end

    if string match -eq 'kitty' -- $TERM
        alias ssh "kitten ssh"
        alias icat "kitten icat"
    end
end
