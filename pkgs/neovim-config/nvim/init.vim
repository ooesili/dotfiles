" pathogen
execute pathogen#infect()

" colors
set termguicolors
let s:colorscheme = 'base16-default-dark'
if !exists('g:colors_name') || g:colors_name != s:colorscheme
  execute 'colorscheme ' . s:colorscheme
endif

" basics
set completeopt=menuone
set cursorline
set expandtab
set ignorecase
set linebreak
set number
set shiftwidth=2
set smartcase
set softtabstop=2
set tabstop=2
set wildmode=longest:full,full

" netrw
let g:netrw_liststyle = 3 " tree view

" ripgrep
if executable("rg")
  set grepprg=rg\ --vimgrep\ --no-heading
  set grepformat=%f:%l:%c:%m,%f:%l:%m
endif

" yank and paste to system clipboard
set clipboard=unnamedplus

" mappings
let mapleader=' '
nnoremap <Leader>e :Explore<CR>
nnoremap <Leader>w :update<CR>
nnoremap <Leader>l :botright copen<CR>
nnoremap <C-e> 5<C-e>
nnoremap <C-y> 5<C-y>
nnoremap g<C-e> <C-e>
nnoremap g<C-y> <C-y>
nnoremap g<C-l> <C-l>
" window navigation
nnoremap <C-h> <C-w><C-h>
nnoremap <C-j> <C-w><C-j>
nnoremap <C-k> <C-w><C-k>
nnoremap <C-l> <C-w><C-l>

" snippets
let g:UltiSnipsExpandTrigger="<c-e>"
let g:UltiSnipsEditSplit="context"
" let g:UltiSnipsSnippetsDir=""

" go
let g:go_fmt_command = "goimports"
let g:go_fmt_fail_silently = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_types = 1
let g:go_highlight_variable_declarations = 1
let g:go_template_file = "@goTemplateFile@"
let g:go_term_enabled = 1

" status line
let g:airline_theme='base16'
let g:airline#extensions#tmuxline#enabled = 0

" terminal
tnoremap <C-l> <nop>
set scrollback=10000
augroup InitVimTerminalSettings
  autocmd!
  autocmd TermOpen * setlocal nonumber
augroup END

" extra filetypes
augroup InitVimFiletypes
  autocmd!
  autocmd BufNewFile,BufRead .envrc set filetype=sh
  autocmd BufNewFile,BufRead Vagrantfile,Berksfile set filetype=ruby
  autocmd BufNewFile,BufRead *.tfvars set filetype=conf
  autocmd BufNewFile,BufRead tmux.conf set filetype=tmux.conf
  autocmd BufNewFile,BufRead *.hcl set filetype=terraform
  autocmd BufNewFile,BufRead *.tfstate setlocal filetype=json shiftwidth=4 softtabstop=4
augroup END

" vim-commentary extensions
augroup InitVimComentary
  autocmd!
  autocmd FileType tf setlocal commentstring=#\ %s
  autocmd FileType sql setlocal commentstring=--\ %s
augroup END

" fzf
let $FZF_DEFAULT_OPTS .= ' --no-height'
set rtp+=~/.fzf
nnoremap <Leader>f :FZF<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>h :History<CR>

" fzf + z
function! s:z(fullscreen, ...)
  if a:0 == 0
    call fzf#run(fzf#wrap('Z', {'source': "bash -c '. @z@; _z 2>&1 | sed \"s/^[0-9.]* *//\"'", 'sink': 'lcd', 'options': '--tac --tiebreak=index'}, a:fullscreen))
  else
    let args = []
    for arg in a:000
      call add(args, shellescape(arg))
    endfor

    let lines = systemlist('. @z@; _z 2>&1 -l ' . join(args) . ' | tail -n 1 | sed "s/^[0-9.]* *//"')
    if len(lines) == 0
      echoerr "z: no match found: "
      return
    endif
    let dir = lines[0]

    execute "lcd " . dir
    echom dir
  endif
endfunction
command! -bang -nargs=* Z call s:z(<bang>0, <f-args>)
nnoremap <Leader>z :Z<CR>

function s:cd()
  execute 'lcd ' . expand('%:h')
  pwd
endfunction
command! CD call s:cd()

command! -nargs=0 Delete call s:delete()
function! s:delete()
  let file = expand("%:p")
  execute 'bdelete '.bufnr('%')
  call delete(file)
endfunction

" ale
let g:ale_lint_on_text_changed = 'never'
let g:ale_linters = {
  \ 'go': ['gofmt', 'go build', 'go vet', 'golint'],
  \ 'ruby': ['ruby'],
  \ 'javascript': ['standard', 'tsserver'],
  \ 'typescript': ['tslint']
  \ }

" direnv
augroup InitVimDirenv
  autocmd!
  autocmd BufWritePost .envrc silent !direnv allow %
augroup END

" terraform
let g:terraform_fmt_on_save = 1
autocmd BufEnter *.tfvars autocmd! terraform BufWritePre *.tfvars

" rust
let g:rustfmt_autosave = 1
let g:rustfmt_fail_silently = 1

" typescript
autocmd BufNewFile,BufRead *.ts,*.tsx setlocal filetype=typescript
