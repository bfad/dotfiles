;;; init.el --- Emacs configuration entry point  -*- lexical-binding: t -*-
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; Getting a warning that this is no longer necessary (package-initialize)

;; Give Customize its own file to write to.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Start with adding packages
(load "~/.emacs.d/init/packages")
(load "~/.emacs.d/init/treesit-setup")
(load "~/.emacs.d/init/cursor_movement")

;; Put anything relating to basic editor behavior and navigation in here
(load "~/.emacs.d/init/editor")
(load "~/.emacs.d/init/appearance")
(load "~/.emacs.d/init/mode-line")
(load "~/.emacs.d/init/terminal")

;; Package configurations
(load "~/.emacs.d/init/completion")
(load "~/.emacs.d/init/consult")
(load "~/.emacs.d/init/embark")
(load "~/.emacs.d/init/projects")

;; Languages and file types
(load "~/.emacs.d/init/languages")

;; The goal is for the first connected client to restore desktop state into its frame rather
;; than an invisible daemon frame getting it. So first frame that connects gets the restored
;; desktop and turns back on `desktop-save-mode`.
(defun my-desktop-init-client ()
  "Restore the desktop when the first client frame appears."
  (remove-hook 'server-after-make-frame-hook #'my-desktop-init-client)
  (desktop-read)
  (desktop-save-mode 1))

(if (daemonp)
    (progn
      (desktop-save-mode 0)
      (add-hook 'server-after-make-frame-hook #'my-desktop-init-client))
  (desktop-save-mode 1))


;; Configure Origami for code folding
;(require 'origami)
;(global-origami-mode)

;; Configure neotree
(use-package neotree
  :bind ([f8] . neotree-toggle)
  :custom (neo-window-fixed-size nil))

;; Show the project in neotree when switching projects, then pick a file.
(defun my-neotree-project-action ()
  "Open the current project root in neotree, then find a file in it."
  (interactive)
  (neotree-dir (project-root (project-current t)))
  (neotree-hide)
  (my-project-find-file))


;; Configure RipGrep via deadgrep
(use-package deadgrep :bind ("C-c s" . deadgrep))

;; Configure hydra
(use-package hydra
  :demand t
  :config
  (load "~/.emacs.d/init/hydra/modes")
  (load "~/.emacs.d/init/hydra/window_management")
  (load "~/.emacs.d/init/hydra/movement")
  (load "~/.emacs.d/init/hydra/cursors")
  (load "~/.emacs.d/init/hydra/folding"))

;; Configure key-chord for use with hydra
(use-package key-chord
  :demand t
  :config
  (key-chord-mode t)
  (key-chord-define-global ";h" 'hydra-modes/body)
  (key-chord-define-global ";w" 'hydra-window/body)
  (key-chord-define-global ";m" 'hydra-movement/body)
  (key-chord-define-global ";c" 'hydra-cursors/body)
  (key-chord-define-global ";f" 'hydra-folding/body))

;; Configure yasnippet
(use-package yasnippet
  :demand t
  :config
  (yas-reload-all))


;; Set Emacs variable exec-path if launching MacOS GUI
;; Useful for MacOS GUI and using ag.el when ag is in /usr/local/bin
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :demand t
  :config
  (exec-path-from-shell-initialize))
