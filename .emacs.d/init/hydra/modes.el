;;; modes.el --- Top-level hydra mode chooser  -*- lexical-binding: t -*-
(defhydra hydra-modes (:exit t)
  "Choose Mode"
  ("w" hydra-window/body "Window Management")
  ("m" hydra-movement/body "Movement")
  ("c" hydra-cursors/body "Cursors")
  ("f" hydra-folding/body "Folding")
  ("q" nil "Quit" :exit t)
)
