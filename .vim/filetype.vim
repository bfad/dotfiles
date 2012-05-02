augroup filetypedetect
    au BufNewFile,BufRead *.lasso setf lassohtml
    au BufNewFile,BufRead *.inc   setf lasso
augroup END

augroup filetypedetect
    au BufNewFile,BufRead .tmux.conf*,tmux.conf* setf tmux
augroup END;
