(defhydra hydra-cursors (:hint nil)
  "Multiple Cursors"

  ("n" mc/mark-next-like-this-word "Next Word")
  ("M-n" mc/mark-next-like-this-word)
  ("p" mc/mark-previous-word-like-this "Previous Word")
  ("M-p" mc/mark-previous-word-like-this)

  ("N" mc/mark-next-like-this-symbol "Next Symbol")
  ("P" mc/mark-previous-symbol-like-this "Previous Symbol")

  ("l" mc/edit-lines "Lines In Region")
  ("M-l" mc/edit-lines)

  ("<down>" mc/mark-next-like-this "Next Line")
  ("<up>" mc/mark-previous-like-this "Previous Line")
  ("C-S-<down>" mc/mark-next-like-this)
  ("C-S-<up>" mc/mark-previous-like-this)
  ;; Setup these bindings in iTerm so behaves like the GUI keys above.
  ("<f13> n" mc/mark-next-like-this)
  ("<f13> p" mc/mark-previous-like-this)

  ("sn" mc/skip-to-next-like-this "Skip & Next")
  ("sp" mc/skip-to-previous-like-this "Skip & Previous")
  ("un" mc/unmark-next-like-this "Undo Last")
  ("up" mc/unmark-previous-like-this "Undo First")

  ("a" mc/mark-all-like-this-dwim "All DWIM")
  ("in" mc/insert-numbers "Insert Numbers")
  ("ia" mc/insert-letters "Insert Letters")

  ("y" yank-rectangle "Yank Rectangle")
  ("m" hydra-movement/body "Movement Mode" :exit t)
  ("q" hydra-modes/body "Quit" :exit t)
)
