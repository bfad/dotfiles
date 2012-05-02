if exists("b:current_syntax")
  finish
endif

if !exists("main_syntax")           
    let main_syntax = 'lasso'
endif

runtime! syntax/html.vim
unlet b:current_syntax

syn include @Lasso <sfile>:p:h/lasso.vim

syntax region lassoRegion matchgroup=lassoRegionDelimiter start=`\[` end=`\]` contains=@Lasso
syntax region lassoRegion matchgroup=lassoRegionDelimiter start=`<?\(=\|lasso\(script\)\?\(\s\|$\)\)` end=`?>` contains=@Lasso

hi def link lassoRegionDelimiter Delimiter 

if main_syntax == 'lasso'
    unlet main_syntax
endif

let b:current_syntax ="lassohtml"
