if status is-interactive
    alias urlencode "string escape --style=url"
    alias urldecode "string unescape --style=url"

    command -q bat; and alias cat bat
    command -q trash; and alias rm trash

    if string match -eq 'kitty' -- $TERM
        alias ssh "kitten ssh"
        alias icat "kitten icat"
    end
end
