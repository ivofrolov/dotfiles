# Dotfiles

This is a repository for bootstrapping the dotfiles with [Dotbot][dotbot] as easy as

```bash
git clone git@github.com:ivofrolov/dotfiles.git .dotfiles && cd .dotfiles && ./install
```

In general, you should be using symbolic links for everything, and using git
submodules whenever possible.

To keep submodules at their proper versions, you could include something like
`git submodule update --init --recursive` in your `install.conf.yaml`.

To upgrade your submodules to their latest versions, you could periodically run
`git submodule update --init --remote`.
