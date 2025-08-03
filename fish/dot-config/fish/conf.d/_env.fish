set -g __fish_git_prompt_show_informative_status 'yes'
set fish_greeting

set --global --export HOMEBREW_PREFIX "/opt/homebrew";
set --global --export HOMEBREW_CELLAR "/opt/homebrew/Cellar";
set --global --export HOMEBREW_REPOSITORY "/opt/homebrew";
fish_add_path --global --path "/opt/homebrew/bin" "/opt/homebrew/sbin";
if test -n "$MANPATH[1]"; set --global --export MANPATH '' $MANPATH; end;
if not contains "/opt/homebrew/share/info" $INFOPATH; set --global --export INFOPATH "/opt/homebrew/share/info" $INFOPATH; end;

set -x EDITOR nano
set -x PAGER less
set -x LESS '-iRF'

if status is-interactive
    if command -s direnv > /dev/null 2>&1
        direnv hook fish | source
    end
end
