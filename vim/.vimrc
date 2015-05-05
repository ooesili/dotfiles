" Maintainer:	Wesley Merkel
"
" When started as "evim", evim.vim will already have done these settings.
if v:progname =~? "evim"
  finish
endif

" Use Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

set history=1000	" keep 1000 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching

" Don't use Ex mode, use Q for formatting
map Q gq

" CTRL-U in insert mode deletes a lot.  Use CTRL-G u to first break undo,
" so that you can undo CTRL-U after inserting a line break.
inoremap <C-U> <C-G>u<C-U>

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

" Only do this part when compiled with support for autocommands.
if has("autocmd")

  " Enable file type detection.
  " Use the default filetype settings, so that mail gets 'tw' set to 72,
  " 'cindent' is on in C files, etc.
  " Also load indent files, to automatically do language-dependent indenting.
  filetype plugin indent on

  " Put these in an autocmd group, so that we can delete them easily.
  augroup vimrcEx
  au!

  " For all text files set 'textwidth' to 78 characters.
  autocmd FileType text setlocal textwidth=78

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  " Also don't do it when the mark is in the first line, that is the default
  " position when opening a file.
  autocmd BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif

  augroup END

else

  set autoindent		" always set autoindenting on

endif " has("autocmd")

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif

" ------------------------- USER SPACE ------------------------- "
" run pathogen
execute pathogen#infect()

" look and feel
set background=dark
colorscheme solarized
set number
set guioptions=ac
set guifont=Terminus\ 9

nmap <Leader>t :TagbarOpen<CR>
nmap <Leader>n :NERDTreeToggle<CR>
nmap <Leader>qc :below cw<CR>
nmap <Leader>ql :below lw<CR>
nmap <Leader>d :bdelete<CR>
nmap <F4> :make clean<CR><CR>
nmap <F5> :make -j9<CR><CR>
nmap <F6> :nmap <buffer> <F5> :><Left>R<Left>C<Left><<Left>><Left>R<Left>C<Left><<Left>
nmap q; A;<Esc>
nmap gb :ls<CR>:b<Space>
nmap gB :ls<CR>:bd<Space>
nmap g/ :%s/
nmap <Leader>w :w<CR>

" surroundings
inoremap "" "<Left>"
inoremap '' '<Left>'
inoremap ( )<Left>(
inoremap { }<Left>{
inoremap [ ]<Left>[
imap <C-l> <Right>
imap ;; <End>;<Esc>
imap <C-b> <CR><Esc>O

" fugitive
nmap <Leader>gr :Git<Space>
nmap <Leader>gs :Gstatus<CR>
nmap <Leader>ge :Gedit<CR>
nmap <Leader>gE :Gedit<Space>
nmap <Leader>gd :Gdiff<CR>
nmap <Leader>gD :Gdiff<Space>
nmap <Leader>gw :Gwrite<CR>
nmap <Leader>gW :Gwrite<Space>
nmap <Leader>gc :Gcommit<CR>
nmap <Leader>gC :Gcommit<Space>

" indent options
set shiftwidth=4
set softtabstop=4
set expandtab
set foldmethod=syntax
set nofoldenable
set hls
set ignorecase smartcase

" diff with saved file function
function! s:DiffWithSaved()
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
command! DS call s:DiffWithSaved()

" NERDCommenter
let NERDComInsertMap='<C-c>'

" Cscope
if has("cscope")
  set csto=0
  set cst
  set nocsverb
  " add any database in current directory
  if filereadable("cscope.out")
    cs add cscope.out
  " else add database pointed to by environment
  elseif $CSCOPE_DB != ""
    cs add $CSCOPE_DB
  endif
  set csverb
  " mappings for easy use
  nmap <C-_>s :cs find s <C-R>=expand("<cword>")<CR><CR>
  nmap <C-_>g :cs find g <C-R>=expand("<cword>")<CR><CR>
  nmap <C-_>c :cs find c <C-R>=expand("<cword>")<CR><CR>
  nmap <C-_>t :cs find t <C-R>=expand("<cword>")<CR><CR>
  nmap <C-_>e :cs find e <C-R>=expand("<cword>")<CR><CR>
  nmap <C-_>f :cs find f <C-R>=expand("<cfile>")<CR><CR>
  nmap <C-_>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
  nmap <C-_>d :cs find d <C-R>=expand("<cword>")<CR><CR>
  nmap <C-_><C-_> :cs add cscope.out<CR>
endif

" SCVim
let g:sclangTerm = "env -u TMUX fork st -e tmux new-session -n sclang"
nmap <Leader>ss :SClangStart<CR>

" powerline configurations
set laststatus=2

" emmet
" only enable for certain files
let g:user_emmet_install_global = 0
autocmd FileType html,css,xml EmmetInstall

" CtrlP
" use regex by default
"let g:ctrlp_regexp = 1
" open in current window by default, but allow a split
let g:ctrlp_open_new_file = 'r'
let g:ctrlp_arg_map = 1
let g:ctrlp_custom_ignore = {
  \ 'dir': '/node_modules$'
  \ }

" javascript-libraries-syntax
let g:used_javascript_libs = 'angularjs,jquery,jasmine,underscore'
