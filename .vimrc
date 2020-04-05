" indentation and spelling
set autoindent
set smartindent
set cindent
set tabstop=4 shiftwidth=4 expandtab
"set spell
"set spelllang=en
set nu
set clipboard=unnamed  " use system clipboard
set hidden

" UTF-8 support
set encoding=utf-8

set nocompatible              " required
filetype off                  " required

"" set the runtime path to include Vundle and initialize
"set rtp+=~/.vim/bundle/Vundle.vim
"call vundle#begin()
"
"" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/.vim/bundle/')
"
"" let Vundle manage Vundle, required
"Plugin 'gmarik/Vundle.vim'
"Plugin 'wakatime/vim-wakatime'
"Plugin 'tmhedberg/SimpylFold'
"Plugin 'vim-scripts/indentpython.vim'
"Plugin 'majutsushi/tagbar'
"Bundle 'Valloric/YouCompleteMe'
"Plugin 'vim-syntastic/syntastic'
"Plugin 'nvie/vim-flake8'
"Plugin 'jnurmine/Zenburn'
"" Plugin 'jceb/vim-orgmode'
"Plugin 'altercation/vim-colors-solarized'
"Plugin 'scrooloose/nerdtree'
"Plugin 'jistr/vim-nerdtree-tabs'
"" Plugin 'ervandew/supertab'
"" Plugin 'BufOnly.vim'
"" Plugin 'wesQ3/vim-windowswap'
"" Plugin 'SirVer/ultisnips'
"" Plugin 'godlygeek/tabular'
"" Plugin 'benmills/vimux'
"" Plugin 'jeetsukumaran/vim-buffergator'
"" Plugin 'gilsondev/searchtasks.vim'
"" Plugin 'Shougo/neocomplete.vim'
"" Plugin 'tpope/vim-dispatch'
"" Plugin 'tpope/vim-speeddating'
"" Plugin 'maksimr/vim-jsbeautify'
"Plugin 'kien/ctrlp.vim'
"" Plugin 'tpope/vim-fugitive'
"Plugin 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}
"Plugin 'preservim/nerdcommenter'
"" Plugin 'jakedouglas/exuberant-ctags'
"Plugin 'universal-ctags/ctags'
"" Plugin 'mmorearty/elixir-ctags'
"" Plugin 'honza/vim-snippets'
"Plugin 'Townk/vim-autoclose'
"" Plugin 'challenger-deep-theme/vim', { 'as': 'challenger-deep' }
"
"" " Elixir Support
"" Plugin 'elixir-lang/vim-elixir'
"" Plugin 'avdgaag/vim-phoenix'
"" Plugin 'mmorearty/elixir-ctags'
"" Plugin 'mattreduce/vim-mix'
"" Plugin 'BjRo/vim-extest'
"" Plugin 'frost/vim-eh-docs'
"" Plugin 'slashmili/alchemist.vim'
"" Plugin 'tpope/vim-endwise'
"" Plugin 'jadercorrea/elixir_generator.vim'
"" Plugin 'mhinz/vim-mix-format'
"
"" " Elm Support
"" Plugin 'lambdatoast/elm.vim'
"
"" " Theme / Interface
"" Plugin 'AnsiEsc.vim'
"" Plugin 'ryanoasis/vim-devicons'
"" Plugin 'vim-airline/vim-airline'
"" Plugin 'vim-airline/vim-airline-themes'
"" Plugin 'sjl/badwolf'
"" Plugin 'tomasr/molokai'
"" Plugin 'morhetz/gruvbox'
"" Plugin 'zenorocha/dracula-theme', {'rtp': 'vim/'}
"" Plugin 'junegunn/limelight.vim'
"" Plugin 'mkarmona/colorsbox'
"" Plugin 'romainl/Apprentice'
"" Plugin 'Lokaltog/vim-distinguished'
"" Plugin 'chriskempson/base16-vim'
"" Plugin 'w0ng/vim-hybrid'
"" Plugin 'AlessandroYorba/Sierra'
"" Plugin 'daylerees/colour-schemes'
"" Plugin 'effkay/argonaut.vim'
"" Plugin 'ajh17/Spacegray.vim'
"" Plugin 'atelierbram/Base2Tone-vim'
"" Plugin 'colepeters/spacemacs-theme.vim'
"" Plugin 'dylanaraps/wal.vim'
"" Plugin 'challenger-deep-theme/vim', { 'as': 'challenger-deep' }
"
"" All of your Plugins must be added before the following line
"call vundle#end()            " required
"filetype plugin indent on    " required
"
"" " Execute Pathogen
"" execute pathogen#infect()
"" syntax on
"" filetype plugin indent on
"
"" Set Tagbar to look for a tags file in the current directory
"set tags=./tags,tags;$HOME
"let g:tagbar_ctags_bin = '/snap/universal-ctags/16/bin/ctags'
"
"" configure YCM
"let g:ycm_autoclose_preview_window_after_completion=1
"map <leader>g   :YcmCompleter GoToDefinitionElseDeclaration<CR>
"
"" Python syntax options
"let python_highlight_all=1
"syntax on
"
"" Fortran syntax options
"let fortran_free_source=1
"let fortran_have_tabs=1
"let fortran_more_precise=1
"let fortran_do_enddo=1
"
"" hide .pyc files in NERDTree
"let NERDTreeIgnore=['\.pyc$', '\~$']
"let g:WebDevIconsUnicodeDecorateFolderNodes = 1
"let g:WebDevIconsNerdTreeAfterGlyphPadding = ' '
"
"" NERDCommenter settings:
"" Add spaces after comment delimiters by default
"let g:NERDSpaceDelims = 1
"
"" Use compact syntax for prefittified multi-line comments
"let g:NERDCompactSexyComs = 1
"
"" Align line-wise comment delimiters flush left instead of following
"" code indentation
"let g:NERDDefaultAlign = 'left'
"
"" Set a language to use its alternate delimiters by default
"let g:NERDAltDelims_java = 1
"
"" Add your own custom formats or override the defaults
"" let g:NERDCustomDelimiters = { 'c': { 'left': '/**', 'right': '*/' } }
"
"" Allow commenting and inverting empty lines (useful when commenting a region
"let g:NERDCommentEmptyLines = 1
"
"" Enable trimming of trailing whitespace when uncommenting
"let g:NERDTrimTrailingWhitespace = 1
"
"" Enable NERDCommenterToggle to check all selected lines is commented
"" or not
"let g:NERDToggleCheckAllLines = 1
"
"" Devicons configuration 
"let g:webdevicons_conceal_nerdtree_brackets = 1
"let g:WebDevIconsNerdTreeAfterGlyphPadding = '  '
"
"" " Elixir Tagbar Configuration
"" let g:tagbar_type_elixir = {
""     \ 'ctagstype' : 'elixir',
""     \ 'kinds' : [
""         \ 'f:functions',
""         \ 'functions:functions',
""         \ 'c:callbacks',
""         \ 'd:delegates',
""         \ 'e:exceptions',
""         \ 'i:implementations',
""         \ 'a:macros',
""         \ 'o:operators',
""         \ 'm:modules',
""         \ 'p:protocols',
""         \ 'r:records',
""         \ 't:tests'
""     \ ]
""     \ }
"
"" set colorscheme
"if has('gui_running')
"    set background=dark
"    colorscheme solarized
"else
"    " colorscheme challenger_deep
"    colorscheme zenburn
"endif
"
"" " switch between light and dark colorschemes by pressing F5
"" call togglebg#map("<F5>")
"
"" " python with virtualenv support
"" py << EOF
"" import os
"" import sys
"" if 'VIRTUAL_ENV' in os.environ:
""   project_base_dir = os.environ['VIRTUAL_ENV']
""   activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
""   execfile(activate_this, dict(__file__=activate_this))
"" EOF
"
"" Enable folding
"set foldmethod=indent
"set foldlevel=99
"
"" Enable folding with spacebar
"nnoremap <space> za
"
"" Invoke NERDTree
"map <C-N> :NERDTreeToggle<CR>
"
"" Tagbar
"map <C-m> :TagbarToggle<CR>
"
"" split navigations
"nnoremap <C-J> <C-W><C-J>
"nnoremap <C-K> <C-W><C-K>
"nnoremap <C-H> <C-W><C-H>
"nnoremap <C-L> <C-W><C-L>
