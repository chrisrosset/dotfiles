" Christopher Rosset
" .vimrc

" {{{ Visual Settings

set number
set lazyredraw
set ttyfast
set listchars=tab:>-,trail:$
set list

syntax on

set t_Co=256

try
    colorscheme jellybeans
catch /^Vim\%((\a\+)\)\=:E185/
    colorscheme desert
endtry

if exists('+colorcolumn')
    set colorcolumn=+1,+11,+21
endif

set laststatus=2
set statusline=
set statusline+=%7*\[%n]                                  "buffernr
set statusline+=%1*\ %<%F\                                "File+path
set statusline+=%2*\ %y\                                  "FileType
set statusline+=%3*\ %{''.(&fenc!=''?&fenc:&enc).''}      "Encoding
set statusline+=%3*\ %{(&bomb?\",BOM\":\"\")}\            "Encoding2
set statusline+=%4*\ %{&ff}\                              "FileFormat (dos/unix..)
"set statusline+=%5*\ %{&spelllang}\%{HighlightSearch()}\  "Spellanguage & Highlight on?
set statusline+=%8*\ %=\ row:%l/%L\ (%03p%%)\             "Rownumber/total (%)
set statusline+=%9*\ col:%03c\                            "Colnr
set statusline+=%0*\ \ %m%r%w\ %P\ \                      "Modified? Readonly? Top/bot.

" }}}
" {{{ GUI Settings

if has('gui_running')
    set guifont=Terminus\ 8,MiscFixed\ 12
    set guioptions-=M " remove filebar
    set guioptions-=M " remove menubar
    set guioptions-=T " remove toolbar
    set guioptions-=r " remove scrollbar
endif

" }}}
" {{{ General Settings

" disables vi compatibility mode
set nocompatible

if has('persistent_undo')
    set undofile
    set undodir=~/.vim/undo
endif

" tab settings
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

" indent settings
filetype plugin indent on
set autoindent

set encoding=utf-8
set scrolloff=5
set showmode
set showcmd
set hidden
set wildmenu
set wildmode=list:longest
set t_vb= " disable error bells
set ruler
set backspace=indent,eol,start
set mouse=a
set splitright " New windows open to the right of the current one
set clipboard^=unnamed

" syntax based folds and opening all folds on loading a file
set foldmethod=syntax
autocmd BufRead * normal zR

" search settings
set ignorecase   " Default to using case insensitive searches,
set smartcase    " unless uppercase letters are used in the regex.
set hlsearch     " Highlight searches by default.
set incsearch    " Incrementally search while typing a /regex
set showmatch    " When a bracket is inserted, briefly jump to the matching one
set matchtime=2  " 10ths of a sec to highlight the showmatch

set nowrap
set textwidth=79
set formatoptions=qrn1
"set formatoptions+=j " remove comment leader when joining lines

function SetTextDocumentSettings()
    setlocal textwidth=0
    setlocal nolist
    setlocal wrap
    setlocal linebreak
endfunction

" }}}
" {{{ Custom Filetype-based Settings

autocmd BufEnter *.md set filetype=markdown | call SetTextDocumentSettings()
autocmd BufEnter *.markdown call SetTextDocumentSettings()
autocmd BufEnter *.rst call SetTextDocumentSettings()
autocmd BufEnter *.tex call SetTextDocumentSettings()

autocmd Filetype java set makeprg=javac\ %
set errorformat=%A%f:%l:\ %m,%-Z%p^,%-C%.%#

" }}}
" {{{ Custom Key-bindings

" set leader key
let mapleader = "\<space>"

" make leader space clear the search highlighting
nnoremap <leader><CR> :noh<cr>

nnoremap <leader>na	:set number<cr>
if exists ('+relativenumber')
    nnoremap <leader>nr	:set relativenumber<cr>
    nnoremap <leader>nn	:set nonumber norelativenumber<cr>
else
    nnoremap <leader>nn	:set nonumber<cr>
endif

" tab keybindings
nnoremap <leader>tn :tabnew<CR>
nnoremap <leader>tc :tabclose<CR>
nnoremap <leader>] :tabnext<CR>
nnoremap <leader>[ :tabprevious<CR>

" compiler keybindings
nnoremap <leader>mm :make<CR>
nnoremap <leader>m[ :cprevious<CR>
nnoremap <leader>m] :cnext<CR>

" markdown headings
nnoremap <leader>h1 yypVr=
nnoremap <leader>h2 yypVr-

nnoremap <leader>w :w<CR>

" set python regexes
"nnoremap / /\v
"vnoremap / /\v

" enables moving cursor by display lines when wrapping
nnoremap j gj
nnoremap k gk

" keeps the visual selection active after indenting
vmap > >gv
vmap < <gv

" enable using semicolon instead of colon to enter command mode
" nnoremap ; :

inoremap jj <ESC>
nnoremap qqq :qa<CR>
nnoremap QQQ :qa!<CR>

" enable the dot command for visual mode (useful for ranges)
xnoremap . :normal .<CR>

" run macro over a visual selection
function! ExecuteMacroOverVisualRange()
  echo "@".getcmdline()
  execute ":'<,'>normal @".nr2char(getchar())
endfunction

xnoremap @ :<C-u>call ExecuteMacroOverVisualRange()<CR>

function TogglePaste()
    if(&paste == 0)
        set paste
        echo "paste"
    else
        set nopaste
        echo "nopaste"
    endif
endfunction

map <F3> :call TogglePaste()<CR>

map <F5> "+y<CR>
map <F6> "+p<CR>

" }}}
" {{{ Custom Commands

cmap w!!! w !sudo tee % > /dev/null

" }}}
" {{{ Plugins and Plugin Settings

try
    set nocompatible " required
    filetype off     " required
    
    set rtp+=~/.vim/bundle/Vundle.vim/
    call vundle#begin()
    Plugin 'gmarik/Vundle.vim'

    Plugin 'tpope/vim-fugitive'
    Plugin 'tpope/vim-surround'
    Plugin 'Raimondi/delimitMate'
    Plugin 'Lokaltog/vim-easymotion'
    "Plugin 'vim-scripts/SearchComplete'
    Plugin 'scrooloose/nerdcommenter'
    Plugin 'mbbill/undotree'
    Plugin 'benmills/vimux'
    Plugin 'kien/ctrlp.vim'

    Plugin 'derekwyatt/vim-fswitch'
    nnoremap <leader>fsh :FSSplitLeft<CR>
    nnoremap <leader>fsj :FSSplitBelow<CR>
    nnoremap <leader>fsk :FSSplitAbove<CR>
    nnoremap <leader>fsl :FSSplitRight<CR>

    Plugin 'majutsushi/tagbar'
    nmap <F8> :TagbarToggle<CR>

    Plugin 'luochen1990/rainbow'
    let g:rainbow_active = 1

    Plugin 'mhinz/vim-startify'
    let g:startify_bookmarks = [ '~/.vimrc', '~/.zshrc' ]

    Plugin 'bling/vim-airline'
    set laststatus=2
    let g:custom_status_line = 1
    let g:airline#extensions#tabline#enabled = 1
    let g:airline#extensions#tabline#left_sep = ' '
    let g:airline#extensions#tabline#left_alt_sep = '|'
    if $BBENV == ""
        let g:airline_powerline_fonts = 1
    endif

    " snipMate
    Plugin 'MarcWeber/vim-addon-mw-utils'
    Plugin 'tomtom/tlib_vim'
    Plugin 'garbas/vim-snipmate'
    Plugin 'honza/vim-snippets'

    " All of your Plugins must be added before the following line
    call vundle#end()         " required
    filetype plugin indent on " required

catch
    echo "Vundle not installed or incorrectly initialized."
    echo "To install run:"
    echo "git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim"
endtry

" }}}

" vim: set foldmethod=marker:
