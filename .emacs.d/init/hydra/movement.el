(defhydra hydra-movement (:hint nil)
  ;; TODO: Larger hint and get rid of hints for individual heads?
  ;; Include something about modifier keys too
  "Movement"
  ("f" forward-char "Forward")
  ("C-f" forward-char)
  ("<right>" forward-char)
  ("M-f" subword-forward)
  ("M-<right>" forward-sexp)

  ("b" backward-char "Backward")
  ("C-b" backward-char)
  ("<left>" backward-char)
  ("M-b" subward-backward)
  ("M-<left>" backward-sexp)

  ("n" next-line "Next")
  ("C-n" next-line)
  ("<down>" next-line)

  ("p" previous-line "Previous")
  ("C-p" previous-line)
  ("<up>" previous-line)

  ("a" smarter-move-beginning-of-line "Start of line")
  ("C-a" smarter-move-beginning-of-line)
  ("s-<left>" smarter-move-beginning-of-line)
  ("e" move-end-of-line "End of Line")
  ("C-e" move-end-of-line)
  ("s-<right>" move-end-of-line)

  ("l" recenter-top-bottom "Re-center")
  ("C-l" recenter-top-bottom)
  ("g" goto-line "Goto Line")

  ("m" set-mark-command "Set Mark" :bind nil)
  ("d" delete-char "Delete")
  ("C-d" delete-char)
  ("w" kill-region "Kill Selection")
  ("C-w" kill-region)
  ("W" kill-ring-save "Copy Selection")
  ("M-w" kill-ring-save)
  ("y" yank "Yank")
  ("C-y" yank)
  ("Y" yank-pop)
  ("M-y" yank-pop)

  ("m" hydra-cursors/body "Multiple Cursors Mode", :exit t)
  ("N" mc/mark-next-like-this-word "MC Next Word")
  ("M-n" mc/mark-next-like-this-word)
  ("P" mc/mark-previous-word-like-this "MC Previous Word")
  ("M-p" mc/mark-previous-word-like-this)
  ("L" mc/edit-lines "MC Lines")
  ("M-l" mc/edit-lines)
  ("C-S-<down>" mc/mark-next-like-this)
  ("C-S-<up>" mc/mark-previous-like-this)
  ;; Setup these bindings in iTerm so behaves like the GUI keys above.
  ("<f13> n" mc/mark-next-like-this)
  ("<f13> p" mc/mark-previous-like-this)

  ("c" hydra-cursors/body "Code Folding Mode", :exit t)
  ("q" hydra-modes/body "Quit" :exit t)
 )
