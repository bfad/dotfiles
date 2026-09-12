;;; languages.el --- Settings for particular languages and file types  -*- lexical-binding: t -*-

(use-package haml-mode :defer t)
(use-package slim-mode :defer t)
(use-package markdown-mode :defer t)
(use-package dockerfile-mode :defer t)
(use-package yaml-mode :defer t)
(use-package nginx-mode :defer t)

;; --- Indentation ---
;; Follow the file being read, and fall back to the settings here.
;;
;; dtrt-indent infers a buffer's real indent offset from its contents and sets
;; the major mode's own indent variable buffer-locally
(use-package dtrt-indent
  :custom
  ;; Tells `dtrt-indent' to run even for modes that use SMIE. This is because SMIE doesn't
  ;; set the mode-specific variables, but `my/indent-shift-width' relies on them.
  (dtrt-indent-run-after-smie t)
  :config
  ;; Modes missing from `dtrt-indent-hook-mapping-list'
  (dolist (entry '((ruby-ts-mode       ruby       ruby-indent-level)
                   (json-ts-mode       javascript json-ts-indent-offset)
                   (yaml-ts-mode       default    yaml-indent-offset)
                   (dockerfile-ts-mode default    dockerfile-indent-offset)))
    (add-to-list 'dtrt-indent-hook-mapping-list entry))
  (dtrt-indent-global-mode 1))

;; --- (S)CSS ---
(use-package rainbow-mode
  :hook (css-base-mode . rainbow-mode))

;; --- JavaScript ---
(setq js-indent-level 2)

;; --- JSON ---
(setq json-ts-indent-offset js-indent-level)

;; --- YAML ---
(setq yaml-indent-offset 2)

;; --- Dockerfile ---
(setq dockerfile-indent-offset 4)

;; --- Templates ---
(use-package web-mode
  :mode "\\.tmpl\\'")

;; --- Git commit messages ---
(add-hook 'git-commit-mode-hook (lambda () (setq-local fill-column 72)))

;; Languages with larger configs
(load "~/.emacs.d/init/ruby")
