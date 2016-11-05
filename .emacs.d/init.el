;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Start with adding packages
(load "~/.emacs.d/init/packages")
(load "~/.emacs.d/init/cursor_movement")

;; Setup UTF-8
(set-default-coding-systems 'utf-8)

;; Turn off the backup files
(setq make-backup-files nil)

;; Highlight matching brackets
(show-paren-mode 1)

;; Auto close brackets
(electric-pair-mode 1)

;; When you select text and then type or paste/yank - delete the selected text
(delete-selection-mode 1)

;; Turn off GUI's tool bar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)


;; Smoother scrolling settings
(setq redisplay-dont-pause t
      scroll-margin 3
      scroll-step 1
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      ;mouse-wheel-progressive-speed nil
      scroll-preserve-screen-position t
      scroll-conservatively 10000
)

;; Problem with this is that it changes selection to be emacs selection instead
;; of Terminal selection. Since Terminal doesn't pass s-c, s-x, or s-v to emacs
;; those commands for copy/cut/paste don't work. (Terminal doesn't see anything)
;; highlighted, and emacs never receives the command. This means I can't use the
;; mouse to highlight text to delete. (Can option click to set cursor location,
;; but can't option click-and-drag and have emacs receive it as a highlight)
;; Turn on mouse mode for terminal
(xterm-mouse-mode t)
;; Turning on xterm-mouse-mode breaks scrolling for some reason
;; This fixes it
(defun my-scroll-up ()
  (interactive)
  (scroll-up 1))
(defun my-scroll-down ()
  (interactive)
  (scroll-down 1))
(global-set-key [mouse-4] 'my-scroll-down)
(global-set-key [mouse-5] 'my-scroll-up)

;; Get horizontal scrolling working for GUI
;; Sort of works - the cursor has to be on the same line that needs scrolling
(global-set-key [wheel-right] (lambda ()
                                (interactive)
                                (scroll-left 1)))
(global-set-key [double-wheel-right] (lambda ()
                                (interactive)
                                (scroll-left 1)))
(global-set-key [triple-wheel-right] (lambda ()
                                (interactive)
                                (scroll-left 1)))
(global-set-key [wheel-left] (lambda ()
                                (interactive)
                                (scroll-right 1)))
(global-set-key [double-wheel-left] (lambda ()
                                (interactive)
                                (scroll-right 1)))
(global-set-key [triple-wheel-left] (lambda ()
                                (interactive)
                                (scroll-right 1)))
;; For some reason, if I start emacs as a daemon, connect to it via the GUI, and
;; then connect to it through the terminal, the kill/yank works with OS X's
;; clipboad. This was the behavior I thought I wanted, but have since reconsidered.
;; following variables are nil. These settings enables the interoperability.
;(setq interprogram-cut-function 'x-select-text)
;(setq interprogram-paste-function 'x-selection-value)
;; This generates an error about the window system not being initialized. There
;; must be other things that the (GUI) sets up that allow for this.
;;
;; Decided to keep the OS X clipboard separate from Emacs' kill ring.
;; Most of this is for the GUI since the terminal keeps separate by default.
(setq interprogram-cut-function nil)
(setq interprogram-paste-function nil)
(defun osx-pbpaste ()
  (interactive)
  (call-process-region (point) (if mark-active (mark) (point)) "pbpaste" t t)
  (setq deactivate-mark t))
(defun osx-pbcopy ()
  (interactive)
  (call-process-region (point) (mark) "pbcopy")
  (setq deactivate-mark t))
(defun osx-pbcut ()
  (interactive)
  (osx-pbcopy)
  (delete-region (region-beginning) (region-end)))
(global-set-key (kbd "s-c") 'osx-pbcopy)
(global-set-key (kbd "s-x") 'osx-pbcut)
(global-set-key (kbd "s-v") 'osx-pbpaste)
;; For iTerm, setup command keys to use the pasteboard. (Pasting is handled by
;; iTerm directly, so I don't need to create that binding.)
(global-set-key (kbd "<f13> c") 'osx-pbcopy)
(global-set-key (kbd "<f13> x") 'osx-pbcut)

;; Multiple-Cursors Key Bindings
;; https://github.com/magnars/multiple-cursors.el
;;
;; For the GUI
(global-set-key (kbd "C-S-<down>") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-<up>") 'mc/mark-previous-like-this)
;; Setup these bindings in iTerm so behaves like the GUI.
(global-set-key (kbd "<f13> n") 'mc/mark-next-like-this)
(global-set-key (kbd "<f13> p") 'mc/mark-previous-like-this)
;; Global
(global-set-key (kbd "M-n") 'mc/mark-next-like-this-word)
(global-set-key (kbd "M-p") 'mc/mark-previous-word-like-this)
(global-set-key (kbd "M-l") 'mc/edit-lines)




;; Similar to what I had before
(global-set-key (kbd "M-U") 'downcase-word)

;; Use spaces for indentation and set default width to 4
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Setup auto-complete
;;(require 'auto-complete-config)
;;(ac-config-default)
;; Start after 1 character
;;(setq ac-auto-start 1)
;; Shorter delay
;;(setq ac-auto-show-menu 0.2)
;;(setq ac-use-fuzzy t)

;; setup company-flx
(with-eval-after-load 'company
  (company-flx-mode +1))

;; Useful settings for code files
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Truncate lines
            (set-default 'truncate-lines t)
            ;; Delete trailing whitespace & end files with a newline
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)
            (setq require-final-newline t)
            ;; Turn line numbering on
            (linum-mode 1)
            ;; Make word commands recognize CamelCase
            (subword-mode)
            ;; Add yasnippet for Textmate-like snippets
            (yas-minor-mode)
            ;; Add company-mode for auto completion previews
            (company-mode)
	    ;; configure company mode
	    (setq company-minimum-prefix-length 1)
	    (setq company-idle-delay 0.3)
           ))

;; Show line and column numbers in the mode line
(line-number-mode 1)
(column-number-mode 1)

;; The linum-mode is turned on for prog-mode, but this creates a function to add
;; a little space between the numbers and the text displayed on the terminal.
;; Since the GUI has the fringe, it doesn't need this extra space
(setq linum-format
      (lambda(line)
        (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
               (propertize (format (format (if (display-graphic-p) "%%%dd" "%%%dd ") w) line) 'face 'linum))))

;(global-linum-mode 1)
;(setq linum-format "%d ")

;; Show backtrace for errors
(setq debug-on-error t)

;; Turn off the alarm for some functions
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit
                        mwheel-scroll my-scroll-up my-scroll-down down up next-line previous-line backward-char forward-char))
          (ding))))

;; Default to making frame as large as possible
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Default fonts setup (though terminals control their own fonts)
;; Done through a hook to get it to work with emacs in daemon mode
;; and using emacsclient -c to launch the GUI
;(add-hook 'after-make-frame-functions
;          #'(lambda (&optional frame)
;              (if frame (select-frame frame))
;              ;;(message (format "%s" (display-graphic-p)))
;; Don't like the daemon mode -- too many things get screwed up
(if (display-graphic-p)
    (progn
      (add-to-list 'default-frame-alist '(font . "Inconsolata-14"))
      (set-frame-font "Inconsolata-14" nil t)
      (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)))
              ;;))


;; Helm Configuration
;; Much of this is taken from Prelude:
;;    https://github.com/bbatsov/prelude/blob/master/modules/prelude-helm.el
;;    https://github.com/bbatsov/prelude/blob/master/modules/prelude-helm-everywhere.el
;;(require 'helm-config)
;;(require 'helm-projectile)
;;
;;(setq helm-split-window-in-side-p           t
;;      helm-buffers-fuzzy-matching           t
;;      helm-move-to-line-cycle-in-source     t
;;      helm-ff-search-library-in-sexp        t
;;      helm-ff-file-name-history-use-recentf t)
;;;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;;;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;;;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
;;(global-set-key (kbd "C-c h") 'helm-command-prefix)
;;(global-unset-key (kbd "C-x c"))
;;
;;(global-set-key (kbd "M-x") 'helm-M-x)
;;(global-set-key (kbd "C-x C-m") 'helm-M-x)
;;(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;;(global-set-key (kbd "C-x b") 'helm-mini)
;;(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
;;(global-set-key (kbd "C-x C-f") 'helm-find-files)
;;(global-set-key (kbd "C-h f") 'helm-apropos)
;;(global-set-key (kbd "C-h r") 'helm-info-emacs)
;;(global-set-key (kbd "C-h C-l") 'helm-locate-library)
;;
;;(define-key helm-command-map (kbd "o")     'helm-occur)
;;(define-key helm-command-map (kbd "g")     'helm-do-grep)
;;(define-key helm-command-map (kbd "C-c w") 'helm-wikipedia-suggest)
;;(define-key helm-command-map (kbd "SPC")   'helm-all-mark-rings)
;;
;;
;;(setq projectile-completion-system 'helm)
;;(helm-descbinds-mode)
;;(helm-mode 1)
;;;;;(helm-projectile-on) ; enable Helm version of Projectile with replacment commands


;; Configure ido
(require 'flx-ido)
(require 'ido-vertical-mode)

;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)

;Colors Make it look like @abo-abo's: http://oremacs.com/2015/02/09/ido-vertical/
;(setq ido-use-faces t)
;(set-face-attribute 'ido-vertical-first-match-face nil
;                    :background "#e5b7c0")
;(set-face-attribute 'ido-vertical-only-match-face nil
;                    :background "#e52b50"
;                    :foreground "white")
;(set-face-attribute 'ido-vertical-match-face nil
;                    :foreground "#b00000")
;(ido-vertical-mode 1)


;; Configure projectile
(projectile-global-mode)
(global-set-key (kbd "s-t") 'projectile-find-file)
(global-set-key (kbd "<f13> t") 'projectile-find-file)

;; Configure Origami for code folding
(require 'origami)
(global-origami-mode)

;; Configure neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; Adapted from neotree-projectile-action function in neotree project
(defun my-neotree-projectile-action ()
  "Integration with `Projectile'.
Usage:
    (setq projectile-switch-project-action 'my-neotree-projectile-action).
When running `projectile-switch-project' (C-c p p), `neotree' will change root
automatically but projectile-find-file will still be called."
  ;;(interactive)
  (cond
   ((fboundp 'projectile-project-root)
    (neotree-dir (projectile-project-root))
    (neotree-hide)
    (projectile-find-file))
   (t
    (error "Projectile is not available"))))
(setq projectile-switch-project-action 'my-neotree-projectile-action)


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

;; Configure scss-mode
(setq scss-compile-at-save nil)

;; Configure yasnippet
(require 'yasnippet)
(yas-reload-all)

;; Configure coffee-mode
;; Configure coffee-mode
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "red3" "green3" "yellow3" "blue2" "magenta3" "cyan3" "gray90"])
 '(coffee-tab-width 2)
 '(cursor-type (quote bar))
 '(desktop-restore-in-current-display t)
 '(desktop-save-mode t)
 '(safe-local-variable-values
   (quote
    ((eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))))))

;; Configure css-mode
;; TODO: Check this works with scss and sass
(add-hook 'css-mode-hook
          (lambda ()
            (rainbow-mode 1)
            ))

;; Load ruby settings
(load "~/.emacs.d/init/ruby")

;; Configure YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Configure dockerfile-mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.dockerfile\\'" . dockerfile-mode))

;; Elm mode
(require 'elm-mode)

;; Alchemist for Elixir
(require 'alchemist)

;; Handlebars templates
(require 'handlebars-mode)

;; Configuration for helm-ag
;(custom-set-variables
;  '(helm-ag-base-command "ack --nocolor --nogroup"))

(load-theme 'material_darker t)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
