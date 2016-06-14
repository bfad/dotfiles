(defhydra hydra-window (:hint nil)
  "Window Management"
  ("v" split-window-right "Split Vertical")
  ("h" split-window-below "Split Horizontal")
  ("d" delete-window "Delete")
  ("1" delete-other-windows "Delete Others")

  ("p" windmove-up "Select Up")
  ("<up>" windmove-up)
  ("n" windmove-down "Select Down")
  ("<down>" windmove-down)
  ("f" windmove-right "Select Right")
  ("<right>" windmove-right)
  ("b" windmove-left "Select Left")
  ("<left>" windmove-left)

  ("=" balance-windows "Equal size")
  ("E" enlarge-window "Expand Verically")
  ("e" enlarge-window-horizontally "Expand Horizontally")
  ("s" shrink-window "Shrink Verically")
  ("S" shrink-window-horizontally "Shrink Horizontally")


  ("q" hydra-modes/body "Quit" :exit t)
)
