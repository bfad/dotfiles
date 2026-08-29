;;; init.git.el --- Minimal init for git commit editing  -*- lexical-binding: t -*-
(package-initialize)
;; Load all package managers
(require 'package)

;;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
;;(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;;(let ((my-packages (list
;;                    'git-commit
;;                    'multiple-cursors
;;                    'exec-path-from-shell
;;                    ))
;;      (package-list-refreshed nil))
;;
;;  ;; If package descriptions have never been downloaded, get them
;;  (or (file-exists-p package-user-dir)
;;      (progn (package-refresh-contents) (setq package-list-refreshed t)))
;;  ;; Activate installed packages
;;  (package-initialize)
;;
;;  ;; Ensure packages are installed
;;  (mapcar
;;   (lambda (package)
;;     (if (package-installed-p package)
;;         nil
;;       (unless package-list-refreshed
;;         (package-refresh-contents)
;;         (setq package-list-refreshed t))
;;       (package-install package)))
;;   my-packages)
;;)

(load "~/.emacs.d/init/cursor_movement")
(load "~/.emacs.d/init/editor")
(load "~/.emacs.d/init/appearance")
(load "~/.emacs.d/init/mode-line")

(setq inhibit-startup-screen t)

(require 'git-commit)

;; Don't collapse diffs from `git commit -v` by default.
;; (Also removes the ability to collapse at all, though.)
(remove-hook 'git-commit-setup-hook #'git-commit-collapse-diff)

(global-git-commit-mode t)


; cutoff for word wrap
(setq-default fill-column 72)
