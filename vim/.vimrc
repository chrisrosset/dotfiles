" disables vi compatibility mode
set nocompatible

filetype plugin indent on

" syntax highlighting
syntax on
set modelines=0

set t_Co=256

try
    colorscheme jellybeans
catch /^Vim\%((\a\+)\)\=:E185/
    colorscheme desert
endtry

" tab settings
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

set encoding=utf-8
set scrolloff=5
set autoindent
set showmode
set showcmd
set hidden
set wildmenu
set listchars=tab:▸\ ,trail:$
set list
set wildmode=list:longest
set visualbell
set ttyfast
set ruler
set backspace=indent,eol,start
set mouse=a

" enables persistent undo
if has('persistent_undo')
    set undofile
    set undodir=~/.vim/undo
endif

set number

" syntax based folds and opening all folds on loading a file
set foldmethod=syntax
au BufRead * normal zR

set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch

set nowrap
set textwidth=79
set formatoptions=qrn1
"set formatoptions+=j " remove comment leader when joining lines

if exists('+colorcolumn')
    set colorcolumn=+1,+11,+21
endif

function SetDocumentSettings()
    set textwidth=0
    set nolist
    set wrap
    set linebreak
    set formatoptions+=aw
endfunction

autocmd BufEnter *.md set filetype=markdown | call SetDocumentSettings()
autocmd BufEnter *.markdown call SetDocumentSettings()
autocmd BufEnter *.rst call SetDocumentSettings()

autocmd Filetype java set makeprg=javac\ %
set errorformat=%A%f:%l:\ %m,%-Z%p^,%-C%.%#

" ============================= CUSTOM KEY-MAPPINGS

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

" enable use of tab to jump between opening/closing elements
nnoremap <tab> %
vnoremap <tab> %

" disables arrow keys
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

" enables moving cursor by display lines when wrapping
nnoremap j gj
nnoremap k gk

" enable using semicolon instead of colon to enter command mode
" nnoremap ; :

inoremap jj <ESC>
nnoremap qqq :qa<CR>

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

" ============================= CUSTOM COMMANDS

cmap w!!! w !sudo tee % > /dev/null


" ============================= STATUS LINE

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


" ============================= STATUS LINE

try
    set rtp+=~/.vim/bundle/vundle/
    call vundle#rc()

    Bundle 'gmarik/vundle'
    Bundle 'tpope/vim-fugitive'
    Bundle 'tpope/vim-surround'
    Bundle 'Raimondi/delimitMate'
    Bundle 'Lokaltog/vim-easymotion'
    Bundle 'vim-scripts/SearchComplete'
    Bundle 'mbbill/undotree'

    Bundle 'kien/rainbow_parentheses.vim'
    try
        au VimEnter * RainbowParenthesesToggle
        au Syntax * RainbowParenthesesLoadRound
        au Syntax * RainbowParenthesesLoadBraces
        au Syntax * RainbowParenthesesLoadSquare
    catch /^Vim\%((\a\+)\)\=:E492/
    endtry

    Bundle 'mhinz/vim-startify'
    let g:startify_bookmarks = [ '~/.vimrc', '~/.zshrc' ]

    Bundle 'bling/vim-airline'
    let g:airline_powerline_fonts = 1

catch /^Vim\%((\a\+)\)\=:E117/
endtry
