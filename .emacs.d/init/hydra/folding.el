;;; folding.el --- Code folding hydra -*- lexical-binding: t -*-

(require 'hideshow)

(defhydra hydra-folding (:hint nil)
  "Code Folding"
  ("c"  hs-hide-block   "Collapse")
  ("o"  hs-show-block   "Open")
  ("t"  hs-cycle        "Cycle")
  ("A"  hs-toggle-all   "Toggle All")
  ("ao" hs-show-all     "All Open")
  ("ac" hs-hide-all     "All Collapsed")
  ("l"  hs-hide-level   "Hide Level")

  ("m" hydra-movement/body "Movement Mode" :exit t)
  ("q" hydra-modes/body "Quit" :exit t)
)
