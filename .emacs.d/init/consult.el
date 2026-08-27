;;; consult.el --- Search and navigate via completing-read  -*- lexical-binding: t -*-
;; Consult is a set of consult-<thing> commands founded on completing-read.

(global-set-key (kbd "C-s")     #'consult-line)
(global-set-key (kbd "s-f")     #'consult-line)
(global-set-key (kbd "C-x b")   #'consult-buffer)
(global-set-key (kbd "M-y")     #'consult-yank-pop)
(global-set-key (kbd "C-x r b") #'consult-bookmark)
(global-set-key (kbd "M-g g")   #'consult-goto-line)
(global-set-key (kbd "M-g M-g") #'consult-goto-line)
(global-set-key (kbd "M-g i")   #'consult-imenu)
(global-set-key (kbd "M-g o")   #'consult-outline)

;; Search again with the previous search string.
(defun my-consult-line-again ()
  "Run `consult-line' with the previous search string."
  (interactive)
  (consult-line (car consult--line-history)))
(global-set-key (kbd "s-g") #'my-consult-line-again)

;; Finding text in a buffer should not use fuzzy matching.
(add-to-list 'completion-category-overrides
             '(consult-location (styles my-orderless-literal)))
