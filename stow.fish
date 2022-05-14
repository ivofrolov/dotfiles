#!/usr/bin/env fish

set -l options (fish_opt --short=h --long=help)
set options $options (fish_opt --short=v --long=verbose)
set options $options (fish_opt --short=n --long=simulate)
set options $options (fish_opt --short=d --long=dir --optional-val)
set options $options (fish_opt --short=t --long=target --optional-val)
argparse --name=(status basename) --stop-nonopt $options -- $argv
or return


function print_help
    echo
    echo -- Stow the package contents
    echo
    echo -- usage: (status basename) [-h] [-dDIR, --dir=DIR] [-tTARGET, --target=TARGET] PACKAGE ...
    echo -- "" -d, --dir \t stow directory, default is current
    echo -- "" -h, --help \t print this help
    echo -- "" -n, --simulate \t do not modify the filesystem
    echo -- "" -t, --target \t target directory, default is stow dir parent
    echo -- "" -v, --verbose \t increase verbosity level
    echo -- "" package \t name in the stow directory
end


function log --inherit-variable _flag_verbose
    argparse --name=(status function) --stop-nonopt "l/level=!_validate_int --min 0 --max 2" -- $argv
    or exit

    if not set -q -l _flag_level
        echo (status function): level option is required >&2
        exit 1
    end

    if test $_flag_level -le (count $_flag_verbose)
        printf $argv >&2
        echo >&2
    end
end


function execute --inherit-variable _flag_simulate
    if set -q _flag_simulate
        echo $argv
    else
        command $argv
    end
end


function stow -a source_package_dir target_package_dir
    for source_path in (find $source_package_dir -type f)
        set -l source_file (string replace $source_package_dir "" $source_path)
        set -l target_file (string replace -a -r "dot-" "." $source_file)
        set -l target_path $target_package_dir$target_file

        if test -L $target_path
            if test $source_path = (realpath $target_path)
                log -l 2 "%s skipped: link already exists" $target_file
            else
                log -l 1 "warning: link to %s exists but points to another file, so skipping" $target_file
            end
            continue
        end

        if test -e $target_path
            log -l 1 "warning: file at %s already exists, so skipping" $target_file
            continue
        end

        if not test -d (dirname $target_path)
            execute mkdir -p (dirname $target_path)
            log -l 2 "%s directory created" (dirname $target_file)
        end

        execute ln -s $source_path $target_path
        log -l 2 "%s linked to %s" $source_file $target_file
    end
end


function resolve_dir -a dir
    set -l resolved (realpath -s (string replace -r "^~" $HOME $dir))
    if not test -d $resolved
        log -l 0 "directory %s doesn't exists" $resolved
        return 1
    end
    echo $resolved
end


if set -q -l _flag_help
    print_help
    return
end

if test (count $argv) -eq 0
    log -l 0 "no packages provided"
    return 1
end

set -l dir $PWD
if set -q -l _flag_dir
    set dir (resolve_dir $_flag_dir)
    or return
end

set -l target (dirname $dir)
if set -q -l _flag_target
    set target (resolve_dir $_flag_target)
    or return
end

for package in $argv
    log -l 2 "stow package %s from %s to %s" $package $dir $target
    stow $dir/$package $target
end
