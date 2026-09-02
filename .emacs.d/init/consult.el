;;; consult.el --- Search and navigate via completing-read  -*- lexical-binding: t -*-
;; Consult is a set of consult-<thing> commands founded on completing-read.
(use-package consult
  ;; Use `:preface` to define at startup (`:config` would define when consult launched.)
  :preface
  ;; Search again with the previous search string.
  (defun my-consult-line-again ()
    "Run `consult-line' with the previous search string."
    (interactive)
    (require 'consult)
    (consult-line (car consult--line-history)))
  :bind (("C-s"     . consult-line)
         ("s-f"     . consult-line)
         ("C-x b"   . consult-buffer)
         ("M-y"     . consult-yank-pop)
         ("C-x r b" . consult-bookmark)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g i"   . consult-imenu)
         ("M-g o"   . consult-outline)
         ("s-g"     . my-consult-line-again))
  :init
  ;; Finding text in a buffer should not use fuzzy matching.
  ;; (NOTE: `my-orderless-literal' is defined in init/completion.el)
  (add-to-list 'completion-category-overrides
               '(consult-location (styles my-orderless-literal))))
