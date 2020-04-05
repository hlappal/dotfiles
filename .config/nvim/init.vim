if !exists('g:vscode')

    call plug#begin()

    Plug 'preservim/nerdtree'
    Plug 'preservim/nerdcommenter'
    Plug 'jiangmiao/auto-pairs'
    Plug 'wakatime/vim-wakatime'
    Plug 'Shougo/deoplete.nvim', { 'do': 'UpdateRemotePlugins' }
    Plug 'zchee/deoplete-jedi'
    Plug 'davidhalter/jedi-vim'
    Plug 'neomake/neomake'
    Plug 'sheerun/vim-polyglot'
    "Plug 'neoclide/coc.nvim', {'branch': 'release'}
    Plug 'ctrlpvim/ctrlp.vim'
    Plug 'jdkanani/vim-material-theme'
    Plug 'sickill/vim-monokai'
    Plug 'sainnhe/gruvbox-material'
    Plug 'morhetz/gruvbox'
    Plug 'dracula/vim', { 'as': 'dracula' }
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    Plug 'vifm/vifm.vim'
    Plug 'ap/vim-css-color'

    call plug#end()

    map <C-n> :NERDTreeToggle<CR>

    set relativenumber
    set nu
    set smarttab
    set cindent
    set tabstop=4
    set shiftwidth=4
    set expandtab
    syntax enable

    set background=dark
    colorscheme gruvbox-material
    "colorscheme dracula

    " Airline theme
    let g:airline_theme='powerlineish'

    " Deoplete settings
    let g:deoplete#enable_at_startup=1
    autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

    " Jedi-Vim settings
    let g:jedi#completions_enabled=0
    let g:jedi#use_splits_not_buffers="right"

    " Neomake settings
    let g:neomake_python_enabled_makers=['pylint']

    " Vifm keybindings
    map <Leader>vv :Vifm<CR>
    map <Leader>vs :VsplitVifm<CR>
    map <Leader>vh :SplitVifm<CR>
    
endif
