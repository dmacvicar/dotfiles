set nocompatible   " Disable vi-compatibility
set encoding=utf-8 " Necessary to show Unicode glyphs
set modeline
set autoread
set esckeys
"set timeoutlen=1000 ttimeoutlen=0
filetype off

if &term =~ '256color'
  " disable Background Color Erase (BCE) so that color schemes
  " render properly when inside 256-color tmux and GNU screen.
  " see also http://snk.tuxfamily.org/log/vim-256color-bce.html
  set t_ut=
endif

" bootstrap
let iCanHazVundle=1
let vundle_readme=expand('~/.vim/bundle/vundle/README.md')
if !filereadable(vundle_readme)
  echo "Installing Vundle.."
  echo ""
  silent !mkdir -p ~/.vim/bundle
  silent !git clone https://github.com/gmarik/vundle ~/.vim/bundle/vundle
  let iCanHazVundle=0
endif
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'
" color scheme
Bundle 'jnurmine/Zenburn'
Bundle 'Lokaltog/vim-powerline'
Bundle 'scrooloose/syntastic'
Bundle 'Shougo/vimproc.vim'
Bundle 'Shougo/unite.vim'
Bundle 'scrooloose/nerdtree'
Bundle 'stephpy/vim-yaml'
Bundle 'wting/rust.vim'
" cfengine3
Bundle 'neilhwatson/vim_cf3'
"...All your other bundles...
if iCanHazVundle == 0
  echo "Installing Bundles, please ignore key map error messages"
  echo ""
  :BundleInstall
endif

filetype plugin indent on
let mapleader=","

" Behavior
"
" tmp files
set nowritebackup
set noswapfile
set nobackup
" search
set hlsearch
set ignorecase
set smartcase
"clipboard
set clipboard=unnamed
if has('unnamedplus')
  set clipboard=unnamed,unnamedplus
endif
" ident
set expandtab
set shiftwidth=2
set synmaxcol=128
set ttyscroll=10
set tabstop=2
set nowrap
set expandtab

" Look and feel
"
set laststatus=2   " Always show the statusline
set lazyredraw
color zenburn
set cursorline
set number

" Highlight trailing whitespace
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

" Automatic formatting
autocmd BufWritePre *.rb :%s/\s\+$//e
autocmd BufWritePre *.go :%s/\s\+$//e
autocmd BufWritePre *.haml :%s/\s\+$//e
autocmd BufWritePre *.html :%s/\s\+$//e
autocmd BufWritePre *.scss :%s/\s\+$//e
autocmd BufWritePre *.slim :%s/\s\+$//e

au BufNewFile * set noeol
au BufRead,BufNewFile *.go set filetype=go
au BufNewFile,BufRead _service set filetype=xml

" more natural splits
set splitbelow
set splitright

" No show command
autocmd VimEnter * set nosc

" Jump to the next row on long lines
map <Down> gj
map <Up>   gk
nnoremap j gj
nnoremap k gk

" format the entire file
nmap <leader>fef ggVG=

" Open new buffers
nmap <leader>s<left>   :leftabove  vnew<cr>
nmap <leader>s<right>  :rightbelow vnew<cr>
nmap <leader>s<up>     :leftabove  new<cr>
nmap <leader>s<down>   :rightbelow new<cr>

" Tab between buffers
noremap <tab> <c-w><c-w>

" Switch between last two buffers
nnoremap <leader><leader> <C-^>

" Resize buffers
if bufwinnr(1)
  nmap Ä <C-W><<C-W><
  nmap Ö <C-W>><C-W>>
  nmap ö <C-W>-<C-W>-
  nmap ä <C-W>+<C-W>+
endif

" NERDTree
nmap <leader>n :NERDTreeToggle<CR>
let NERDTreeHighlightCursorline=1
let NERDTreeIgnore = ['tmp', '.yardoc', 'pkg']

" if first argument is a directory, open
" nerdtree
if isdirectory(argv(0))
  bd
  autocmd vimenter * exe "cd" argv(0)
  autocmd VimEnter * NERDTree
endif

" Syntastic
let g:syntastic_mode_map = { 'mode': 'passive' }
let g:syntastic_ruby_exec = '~/.rvm/rubies/ruby-2.0.0-p0/bin/ruby'

" Unite
nnoremap <C-f> :Unite file_rec/async<cr>
nnoremap <C-p> :Unite buffer<cr>

" Quit with :Q
command -nargs=0 Quit :qa!
