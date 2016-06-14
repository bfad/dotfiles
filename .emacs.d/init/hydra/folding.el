(defhydra hydra-folding (:hint nil)
  "Code Folding"
  ("c" origami-close-node "Collapse")
  ("o" origami-open-node "Open")
  ("t" origami-recursively-toggle-node "Toggle")
  ("ao" origami-open-all-nodes "All Open")
  ("ac" origami-close-all-nodes "All Collapsed")

  ("m" hydra-movement/body "Movement Mode" :exit t)
  ("q" hydra-modes/body "Quit" :exit t)
)
