function fish_title
    if set -q argv[1]
        echo -- (prompt_pwd) — (string sub -l 20 -- $argv[1])
    else
        set -l command (status current-command)
        echo -- (prompt_pwd) — (string sub -l 20 -- $command)
    end
end
