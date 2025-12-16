set -g __fish_git_prompt_show_informative_status 'yes'
set fish_greeting

/opt/homebrew/bin/brew shellenv | source

set -x EDITOR nano
set -x PAGER less
set -x LESS '-iRF'

if status is-interactive
    if command -s direnv > /dev/null 2>&1
        direnv hook fish | source
    end
end
