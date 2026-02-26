set -g __fish_git_prompt_show_informative_status 'yes'
set fish_greeting

command -q brew; or /opt/homebrew/bin/brew shellenv | source

set -x EDITOR nano
set -x PAGER less
set -x LESS '-iRF'

if status is-interactive
    command -q direnv; and direnv hook fish | source
end
