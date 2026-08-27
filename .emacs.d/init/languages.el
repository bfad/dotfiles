;;; languages.el --- Settings for particular languages and file types  -*- lexical-binding: t -*-

;; Languages with larger configs
(load "~/.emacs.d/init/ruby")

;; --- (S)CSS ---
(add-hook 'css-base-mode-hook
          (lambda ()
            (rainbow-mode 1)
            ))

;; --- JavaScript ---
(setq js-indent-level 2)

;; --- Templates ---
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))

;; --- Git commit messages ---
(add-hook 'git-commit-mode-hook (lambda () (setq-local fill-column 72)))
