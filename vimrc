set nocompatible
set autoindent
filetype plugin indent on
set cindent
set smartindent
set vb t_vb=
set ruler
set incsearch
set number
set ic 
set expandtab
set tabstop=4

" Colors
set cul                                           " highlight current line
hi CursorLine ctermbg=236      " adjust color
syntax on

" Quick tabbing
map ,t :s/^/\t/<CR>
map ,2t :s/^/\t\t/<CR>
map ,3t :s/^/\t\t\t/<CR>
map ,4t :s/^/\t\t\t\t/<CR>

" custom commands for mass comment and uncomment
map ,# :s/^/#/<CR>:set nohlsearch<CR>

" ,/ C/C++/C#/Java // comments
map ,/ :s/^/\/\//<CR>:set nohlsearch<CR>

" ,< HTML comment
map ,< :s/^\(.*\)$/<!-- \1 -->/<CR>:nohlsearch<CR>

" Press F4 to toggle highlighting on/off, and show current value.
:noremap <F4> :set hlsearch! hlsearch?<CR>

" Saving me from myself
:command WQ wq
:command Wq wq
:command W w
:command Q q

"Context sensitive tab completion
function! Tab_Or_Complete()
	if col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~ '^\w'
endfunction
