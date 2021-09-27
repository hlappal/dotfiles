source $HOME/.config/nvim/general/settings.vim
source $HOME/.config/nvim/keys/mappings.vim

"if !exists('g:vscode')
"
"  call plug#begin()
"
"  " A tree explorer plugin for vim
"  Plug 'preservim/nerdtree'
"
"  " Intensely nerdy commenting powers
"  Plug 'preservim/nerdcommenter'
"
"  " Insert or delete brackets, parens, quotes in pair
"  Plug 'jiangmiao/auto-pairs'
"
"  " Asynchronous linting and make framework for Neovim/Vim
"  Plug 'neomake/neomake'
"
"  " A solid language pack for Vim
"  Plug 'sheerun/vim-polyglot'
"
"  " Intellisense engine for Vim8 & Neovim, full language server protocol support as VSCode 
"  Plug 'neoclide/coc.nvim', {'branch': 'release'}
"
"  " Fuzzy file, buffer, mru, tag, etc finder
"  Plug 'ctrlpvim/ctrlp.vim'
"
"  " Lean & mean status/tabline for vim that's light as air
"  Plug 'vim-airline/vim-airline'
"
"  " A collection of themes for vim-airline
"  Plug 'vim-airline/vim-airline-themes'
"
"  "Plug 'vifm/vifm.vim'
"
"  " Preview colours in source code while editing
"  Plug 'ap/vim-css-color'
"
"  " A calendar application for Vim
"  Plug 'itchyny/calendar.vim'
"
"  " The ultimate snippet solution for Vim
"  Plug 'SirVer/ultisnips'
"
"  " Vim-snipmate default snippets
"  Plug 'honza/vim-snippets'
"
"  " A modern vim plugin for editing LaTeX files
"  Plug 'lervag/vimtex'
"
"  " Colorschemes
"  Plug 'jdkanani/vim-material-theme'
"  Plug 'sickill/vim-monokai'
"  Plug 'sainnhe/gruvbox-material'
"  Plug 'morhetz/gruvbox'
"  Plug 'dracula/vim', { 'as': 'dracula' }
"  Plug 'arcticicestudio/nord-vim'
"  Plug 'dikiaap/minimalist'
"
"  call plug#end()
"
"  map <C-n> :NERDTreeToggle<CR>
"
"  set relativenumber
"  set nu
"  set smarttab
"  set cindent
"  set tabstop=4
"  set shiftwidth=4
"  set expandtab
"  set nocompatible
"  filetype plugin on
"  syntax on
"  set t_Co=256
"
"  "set background=dark
"  colorscheme nord
"
"  " Airline theme
"  let g:airline_theme='nord'
"
"  " Deoplete settings
"  "let g:deoplete#enable_at_startup=1
"  "autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif
"
"  " Jedi-Vim settings
"  "let g:jedi#completions_enabled=0
"  "let g:jedi#use_splits_not_buffers="right"
"
"  " Neomake settings
"  "let g:neomake_python_enabled_makers=['pylint']
"
"  " Trigger UltiSnips configuration
"  let g:UltiSnipsExpandTrigger="<tab>"
"  let g:UltiSnipsJumpForwardTrigger="<c-b>"
"  let g:UltiSnipsJumpBackwardTrigger="<c-z>"
"  let g:UltiSnipsEditSplit="vertical"
"
"  " Vimtex call deoplete
"  "call deoplete#custom#var('omni', 'input_patterns', {
"              "\ 'tex': g:vimtex#re#deoplete
"              "\})
"
"  " Vifm keybindings
"  map <Leader>vv :Vifm<CR>
"  map <Leader>vs :VsplitVifm<CR>
"  map <Leader>vh :SplitVifm<CR>
"
"
""  """"""""""""""""""""""""""""""""""""""""
""  " COC settings 
""  """"""""""""""""""""""""""""""""""""""""
""  
""  " TextEdit might fail if hidden is not set.
""  set hidden
""
""  " Some servers have issues with backup files, see #649.
""  set nobackup
""  set nowritebackup
""
""  " Give more space for displaying messages.
""  set cmdheight=2
""
""  " Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
""  " delays and poor user experience.
""  set updatetime=300
""
""  " Don't pass messages to |ins-completion-menu|.
""  set shortmess+=c
""
""  " Always show the signcolumn, otherwise it would shift the text each time
""  " diagnostics appear/become resolved.
""  if has("patch-8.1.1564")
""    " Recently vim can merge signcolumn and number column into one
""    set signcolumn=number
""  else
""    set signcolumn=yes
""  endif
""
""  " Use tab for trigger completion with characters ahead and navigate.
""  " NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
""  " other plugin before putting this into your config.
""  inoremap <silent><expr> <TAB>
""        \ pumvisible() ? "\<C-n>" :
""        \ <SID>check_back_space() ? "\<TAB>" :
""        \ coc#refresh()
""  inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
""
""  function! s:check_back_space() abort
""    let col = col('.') - 1
""      return !col || getline('.')[col - 1]  =~# '\s'
""  endfunction
""
""  " Use <c-space> to trigger completion.
""  inoremap <silent><expr> <c-space> coc#refresh()
""
""  " Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
""  " position. Coc only does snippet and additional edit on confirm.
""  " <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
""  if exists('*complete_info')
""    inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
""  else
""    inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
""  endif
""
""  " Use `[g` and `]g` to navigate diagnostics
""  nmap <silent> [g <Plug>(coc-diagnostic-prev)
""  nmap <silent> ]g <Plug>(coc-diagnostic-next)
""
""  " GoTo code navigation.
""  nmap <silent> gd <Plug>(coc-definition)
""  nmap <silent> gy <Plug>(coc-type-definition)
""  nmap <silent> gi <Plug>(coc-implementation)
""  nmap <silent> gr <Plug>(coc-references)
""
""  " Use K to show documentation in preview window.
""  nnoremap <silent> K :call <SID>show_documentation()<CR>
""
""  function! s:show_documentation()
""    if (index(['vim','help'], &filetype) >= 0)
""      execute 'h '.expand('<cword>')
""    else
""      call CocAction('doHover')
""    endif
""  endfunction
""
""  " Highlight the symbol and its references when holding the cursor.
""  autocmd CursorHold * silent call CocActionAsync('highlight')
""
""  " Symbol renaming.
""  nmap <leader>rn <Plug>(coc-rename)
""
""  " Formatting selected code.
""  xmap <leader>f  <Plug>(coc-format-selected)
""  nmap <leader>f  <Plug>(coc-format-selected)
""
""  augroup mygroup
""    autocmd!
""    " Setup formatexpr specified filetype(s).
""    autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
""    " Update signature help on jump placeholder.
""    autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
""  augroup end
""
""  " Applying codeAction to the selected region.
""  " Example: `<leader>aap` for current paragraph
""  xmap <leader>a  <Plug>(coc-codeaction-selected)
""  nmap <leader>a  <Plug>(coc-codeaction-selected)
""  
""  " Remap keys for applying codeAction to the current buffer.
""  nmap <leader>ac  <Plug>(coc-codeaction)
""  " Apply AutoFix to problem on the current line.
""  nmap <leader>qf  <Plug>(coc-fix-current)
""  
""  " Map function and class text objects
""  " NOTE: Requires 'textDocument.documentSymbol' support from the language server.
""  xmap if <Plug>(coc-funcobj-i)
""  omap if <Plug>(coc-funcobj-i)
""  xmap af <Plug>(coc-funcobj-a)
""  omap af <Plug>(coc-funcobj-a)
""  xmap ic <Plug>(coc-classobj-i)
""  omap ic <Plug>(coc-classobj-i)
""  xmap ac <Plug>(coc-classobj-a)
""  omap ac <Plug>(coc-classobj-a)
""  
""  " Use CTRL-S for selections ranges.
""  " Requires 'textDocument/selectionRange' support of LS, ex: coc-tsserver
""  nmap <silent> <C-s> <Plug>(coc-range-select)
""  xmap <silent> <C-s> <Plug>(coc-range-select)
""  
""  " Add `:Format` command to format current buffer.
""  command! -nargs=0 Format :call CocAction('format')
""  
""  " Add `:Fold` command to fold current buffer.
""  command! -nargs=? Fold :call     CocAction('fold', <f-args>)
""  
""  " Add `:OR` command for organize imports of the current buffer.
""  command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')
""  
""  " Add (Neo)Vim's native statusline support.
""  " NOTE: Please see `:h coc-status` for integrations with external plugins that
""  " provide custom statusline: lightline.vim, vim-airline.
""  set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}
""  
""  " Mappings using CoCList:
""  " Show all diagnostics.
""  nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
""  " Manage extensions.
""  nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
""  " Show commands.
""  nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
""  " Find symbol of current document.
""  nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
""  " Search workspace symbols.
""  nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
""  " Do default action for next item.
""  nnoremap <silent> <space>j  :<C-u>CocNext<CR>
""  " Do default action for previous item.
""  nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
""  " Resume latest coc list.
""  nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
"    
"endif
