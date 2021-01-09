if [ -f ~/.bash_aliases ]
    . ~/.bash_aliases
end

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /home/hlappal/anaconda3/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

# Enable Starship prompt
#starship init fish | source
