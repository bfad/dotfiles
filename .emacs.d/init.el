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
;; macOS binds s-k to kill-current-buffer by default; free it up so it can
;; act as a prefix key (VS Code style s-k s-b to toggle the sidebar).
(global-unset-key (kbd "s-k"))

(use-package neotree
  :bind (([f8] . neotree-toggle)
         ("s-k s-b" . neotree-toggle)
         ("<f12> k b" . neotree-toggle))
  :custom
  (neo-window-fixed-size nil)
  (neo-show-hidden-files t))

;; Show the project in neotree when switching projects, then pick a file.
(defun my-neotree-project-action ()
  "Open the current project root in neotree, then find a file in it."
  (interactive)
  (neotree-dir (project-root (project-current t)))
  (neotree-hide)
  (my-project-find-file))


;; Configure magit
;; Magit buffers that open beside the current window (status, diffs, revisions,
;; process output) go in the leftmost window instead of to the right.
(defun my-display-buffer-left (buffer alist)
  "Display BUFFER in the leftmost window, just right of any side windows.
If there are side-by-side windows, reuse the leftmost one in the selected
window's row, whichever window has focus.  Otherwise (a single window, or
only windows stacked above each other) split off a new full-height window on
the left.  Side windows such as neotree are never used; if one (or the
minibuffer) is selected, the most recently used regular window stands in for
it.  If nothing works, return nil so `display-buffer' carries on with its
default behavior."
  (let ((window (if (or (window-parameter nil 'window-side)
                        (window-minibuffer-p))
                    (get-mru-window nil nil t)
                  (selected-window)))
        next)
    (when window
      ;; Walk left to the frame edge, stopping short of side windows.
      (while (and (setq next (window-in-direction 'left window))
                  (not (window-parameter next 'window-side)))
        (setq window next))
      (setq next (window-in-direction 'right window))
      (if (and next
               (not (window-parameter next 'window-side))
               (not (window-dedicated-p window)))
          (window--display-buffer buffer window 'reuse alist)
        (when-let* ((new (split-window-no-error (window-main-window) nil 'left)))
          (window--display-buffer buffer new 'window alist))))))

(defun my-magit-display-buffer-left (buffer)
  "Display BUFFER like `magit-display-buffer-traditional', but on the left.
Buffers magit shows in the current window are unaffected.  For the rest, reuse
a window already showing BUFFER, else use `my-display-buffer-left'."
  (let ((display-buffer-base-action
         '((display-buffer-reuse-window my-display-buffer-left))))
    (magit-display-buffer-traditional buffer)))

(use-package magit
  :defer t
  :custom (magit-display-buffer-function #'my-magit-display-buffer-left))


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
