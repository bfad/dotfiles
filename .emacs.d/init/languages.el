;;; languages.el --- Settings for particular languages and file types  -*- lexical-binding: t -*-

(use-package haml-mode :defer t)
(use-package slim-mode :defer t)
(use-package markdown-mode :defer t)
(use-package dockerfile-mode :defer t)
(use-package yaml-mode :defer t)
(use-package nginx-mode :defer t)

;; --- (S)CSS ---
(use-package rainbow-mode
  :hook (css-base-mode . rainbow-mode))

;; --- JavaScript ---
(setq js-indent-level 2)

;; --- Templates ---
(use-package web-mode
  :mode "\\.tmpl\\'")

;; --- Git commit messages ---
(add-hook 'git-commit-mode-hook (lambda () (setq-local fill-column 72)))

;; Languages with larger configs
(load "~/.emacs.d/init/ruby")
