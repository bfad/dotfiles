" Use Vim settings, rather than Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible


""""""""""""""""""""""
" Plugin Information "
""""""""""""""""""""""
"
" delimitMate
"   Provides insert mode auto-completion for quotes, parens, brackets, etc.
"       (https://github.com/Raimondi/delimitMate)
"       (http://www.vim.org/scripts/script.php?script_id=2754)
"
" surround
"   Provides mappings to easily delete, change and add surrounding pairs.
"       (https://github.com/tpope/vim-surround)
"       (http://www.vim.org/scripts/script.php?script_id=1697)
"
" Command-T
"   Provides a Textmate-like ability to open files / buffers in few keystrokes.
"       (https://wincent.com/products/command-t)
"       (https://github.com/wincent/Command-T)
"
" snipMate
"   Provides some of TextMate's snippets features in Vim.
"       (https://github.com/msanders/snipmate.vim)
"       (http://www.vim.org/scripts/script.php?script_id=2540)
"   Got snippets from https://github.com/scrooloose/snipmate-snippets
"       $> git clone https://github.com/scrooloose/snipmate-snippets.git
"       $> cd snipmate-snippets
"       $> rake deploy_local
"
" supertab
"   Allows for all your insert completion (|ins-completion|) with <Tab>.
"       (https://github.com/ervandew/supertab)


"""""""""""""""""""""""""""""
" General Behavior Settings "
"""""""""""""""""""""""""""""
set history=500	" keep 500 lines of command line history
set ruler		" show the cursor line,column position all the time
set number      " show line numbers
set nowrap      " don't wrap text
set showcmd		" display incomplete commands
set incsearch	" do incremental searching (See first found match as you search)
set autoread    " if detect changes in the file from outside, read the file again
set ignorecase  " case-insensetive searching
set smartcase   " unless I use an uppercase, then be case-sensetive
set wildmenu    " enhanced command-line completion - display options when hit tab

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif

" Map leader for help with quick command shortcuts
let mapleader = ","


""""""""""""""""""""
" Vim Helper Files "
""""""""""""""""""""
" Turn on undo persistence (wrap because very new feature)
try
    set undodir=~/.vim_tmp/undo
    set undofile
catch
endtry

" Set where to save swap files 
" (End path with // to include path in swap filename)
set directory=~/.vim_tmp/swap//,~/tmp//,/var/tmp//,/tmp//

" Location of command history / other info
set viminfo +=n~/.vim_tmp/.viminfo

" Don't keep backup files
set nobackup


"""""""""""""""""""""""
" Whitespace Settings "
"""""""""""""""""""""""
set tabstop     =4  " Number of spaces a tab appears indented
set softtabstop =4  " Number of spaces that count as a tab
set shiftwidth  =4  " Number of spaces to use for indent / outdent
set expandtab       " Use spaces instead of tabs
set shiftround      " Always indent / outdent to the nearest shiftwidth setting
set autoindent      " Automatically keep lines indented
set smartindent     " Try to be smart about keeping lines indented
set cpoptions  +=I  " Leave indent from 'autoindent' even if move the cursor up or down

" allow backspacing over everything in insert mode
set backspace=indent,eol,start

" Enable file type detection and load indent files for lanuage-dependent indenting
filetype plugin indent on

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


""""""""""""""""""""
" Status Bar Setup "
""""""""""""""""""""
set statusline=[%Y]\ %<%f\ %=%w%h%r%m%-14.(%l,%c%V%)\ %p
set laststatus=2


""""""""""""""""""""" 
" Highlight Options "
""""""""""""""""""""" 
syntax on               " syntax highlighting turned on
set t_Co=256            " syntax highlighting can use 256 colors (my terminal supports it)
set background=dark     " syntax color theme has dark background
colorscheme jellybeans  " syntax color theme to use
set hlsearch            " highlight search results
set cursorline          " highlight the line the cursor is on


""""""""""""""
" Key Remaps "
""""""""""""""
" Easier to get to type a command, but I still have access to a semi-colon
noremap ; :
noremap : ;   

" Emacs bindings subset
" (Note: noremap! = Normal / Command-line modes)
" (Note: vnoremap = Visual / Select modes)
" (See ":h map" and "h map-overview" for more info)
"
" Forward Delete character / line
noremap! <C-d> <C-o>x
noremap! <C-k> <C-o>D
" Yank
noremap! <C-y> <C-o>p
"
" Movement left / right
noremap! <C-b> <Left>
noremap! <C-f> <Right>
" Movement line start / end
noremap! <C-a> <Esc>I
noremap! <C-e> <Esc>A
"
" Selection left / right
noremap! <C-S-f> <Esc>v<Right>
noremap! <C-S-b> <Esc>v<Left>
vnoremap <C-S-f> <Right>
vnoremap <C-S-b> <Left>
" Selection line start / end
noremap! <C-S-a> <Esc>v0
noremap! <C-S-e> <Esc>v$
vnoremap <C-S-a> 0
vnoremap <C-S-e> $

" OS X Standard Editing Keybindings
" (Often faster than swtiching to Normal mode just to move a bit)
" (Can't do shortcuts to start & end of line as Teriminal / iTerm2 doesn't send command-key)
"
" Move forward / backward a word
noremap! <T-Right> <C-o>e<Right>
noremap! <T-Left> <C-o>b
" Move start / end of line
"
" Select left / right
noremap! <S-Left> <Esc>v<Left>
noremap! <S-Right> <Esc>v<Right>
vnoremap <S-Left> <Left>
vnoremap <S-Right> <Right>
" Select word forward / backward
noremap! <T-S-Left> <C-o>vb
noremap! <T-S-Right> <C-o>ve
vnoremap <T-S-Left> b
vnoremap <T-S-Right> e


"""""""""""""""""
" Set Variables "
"""""""""""""""""
let g:sql_type_default = "mysql"  " Set the default SQL syntax for highlighting to MySQL


"""""""""""""""""""
" Custom Commands "
"""""""""""""""""""
" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif


"""""""""""""""""""""""""""""
" GUI Settings              "
" (here instead of .gvimrc) "
"""""""""""""""""""""""""""""
if has('gui_running')
    set guioptions-=T           " remove the toolbar
    set guifont=Inconsolata:h14,Menlo\ Regular:h12,Consolas:h14
endif


" Reload .vimrc if changes are made to it
" From http://vim.wikia.com/wiki/Change_vimrc_with_auto_reload
autocmd! bufwritepost .vimrc source %
