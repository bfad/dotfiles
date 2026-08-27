;;; packages.el --- Package archives and package list  -*- lexical-binding: t -*-
;; Load all package managers
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(let ((my-packages (list
                    's           ; string helpers, used by init/terminal.el
                    'magit
                    'multiple-cursors
                    'visual-fill-column
                    ; SEEMS TO BE BROKEN 'origami
                    'key-chord
                    'deadgrep
                    'exec-path-from-shell
                    'neotree
                    'hydra
                    'avy
                    'vertico
                    'orderless
                    'marginalia
                    'consult
                    'embark
                    'embark-consult
                    'corfu
                    'cape
                    'yasnippet
                    'web-mode
                    'chruby
                    'inf-ruby
                    'rspec-mode
                    'haml-mode
                    'slim-mode
                    'markdown-mode
                    'rainbow-mode
                    'dockerfile-mode
                    'yaml-mode
                    'nginx-mode
                    ))
      (package-list-refreshed nil))

  ;; If package descriptions have never been downloaded, get them
  (or (file-exists-p package-user-dir)
      (progn (package-refresh-contents) (setq package-list-refreshed t)))
  ;; Activate installed packages
  (package-initialize)

  ;; Ensure packages are installed
  ;; Guard each installation so that an install error gives a warning at the end
  ;; rather than leave Emacs half-configured.
  (let (failed)
    (dolist (package my-packages)
      (unless (package-installed-p package)
        (unless package-list-refreshed
          (package-refresh-contents)
          (setq package-list-refreshed t))
        (condition-case err
            (package-install package)
          (error
           (push (cons package (error-message-string err)) failed)))))
    (when failed
      (display-warning
       'init
       (concat "These packages failed to install:\n"
               (mapconcat (lambda (f) (format "  %s: %s" (car f) (cdr f)))
                          (nreverse failed) "\n"))
       :warning)))
)
