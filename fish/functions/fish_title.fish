function fish_title
    set -l command
    if set -q argv[1]
        set command $argv[1]
    else
        set command (status current-command)
    end
    echo -- (prompt_pwd) — (string replace -r '^(.{16})(.+)(.{16})$' '$1…$3' $command)
end
