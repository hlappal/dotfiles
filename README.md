# My config files

A nice way to keep my config files organized and safe as suggested in this [blog post](https://www.atlassian.com/git/tutorials/dotfiles).

## Installation to new system

Make sure you have committed the alias to you .bashrc or .zshrc:

```bash
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
```

Make your source repository ignore the folder, so that you don't create weird recursion problems:

```bash
echo ".cfg" >> .gitignore
```

Clone your dotfiles into a bare repository a /dot/ folder of you $HOME:

```bash
git clone --bare <git-repo-url> $HOME/.cfg
```

Define the alias in the current shell scope:

```bash
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
```

Checkout the actual content from the bare repository to you $HOME:

```bash
config checkout
```

Set the flag `showUntrackedFiles` to `no` on this specific (local) repository:

```bash
config config --local status.showUntrackedFiles no
```

You're done. From now on you can type `config` commands as you would any `git` commands:

```bash
config status
config add .vimrc
config commit -m "Added vimrc"
config push
```
