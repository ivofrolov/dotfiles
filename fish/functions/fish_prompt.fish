function fish_prompt --description 'Write out the prompt'
    set -l last_pipestatus $pipestatus[-1]

    set -l color_status $fish_color_user
    if test $last_pipestatus != 0
        set color_status $fish_color_error
    end

    set -l normal (set_color normal)

    set -l suffix '❯'

    echo -n -s (set_color $fish_color_cwd) (prompt_pwd) $normal " " (set_color $color_status) $suffix $normal " "

end
