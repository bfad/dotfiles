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

;; Emacs desktop (saves state to reload when reopening)
;; When running an emacs daemon, start it with `--no-desktop` option so it won't
;; load the desktop frames itself. To load saved desktop in GUI client, run:
;; `emacsclient -n -c -e '(progn (desktop-save-mode 1) (desktop-read))`
(setq desktop-restore-in-current-display t)
(desktop-save-mode 1)


;; Configure Origami for code folding
;(require 'origami)
;(global-origami-mode)

;; Configure neotree
(global-set-key [f8] 'neotree-toggle)
(setq neo-window-fixed-size nil)

;; Show the project in neotree when switching projects, then pick a file.
(defun my-neotree-project-action ()
  "Open the current project root in neotree, then find a file in it."
  (interactive)
  (neotree-dir (project-root (project-current t)))
  (neotree-hide)
  (my-project-find-file))


;; Configure RipGrep via deadgrep
(global-set-key (kbd "C-c s") #'deadgrep)

;; Configure hydra
(require 'hydra)
(load "~/.emacs.d/init/hydra/modes")
(load "~/.emacs.d/init/hydra/window_management")
(load "~/.emacs.d/init/hydra/movement")
(load "~/.emacs.d/init/hydra/cursors")
(load "~/.emacs.d/init/hydra/folding")

;; Configure key-chord for use with hydra
(require 'key-chord)
(key-chord-mode t)
(key-chord-define-global ";h" 'hydra-modes/body)
(key-chord-define-global ";w" 'hydra-window/body)
(key-chord-define-global ";m" 'hydra-movement/body)
(key-chord-define-global ";c" 'hydra-cursors/body)
(key-chord-define-global ";f" 'hydra-folding/body)

;; Configure yasnippet
(require 'yasnippet)
(yas-reload-all)


;; Set Emacs variable exec-path if launching MacOS GUI
;; Useful for MacOS GUI and using ag.el when ag is in /usr/local/bin
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
