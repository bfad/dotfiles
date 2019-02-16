;; Configuration of basic editor behavior. This includes keyboard and mouse
;; navigation, common editing options (such as balancing parens), etc.

;; Setup UTF-8
(set-default-coding-systems 'utf-8)

;; Turn off the backup files
(setq make-backup-files nil)

;; Automatically revert buffers if their files were changed externally
(global-auto-revert-mode t)

;; Highlight matching brackets
(show-paren-mode 1)

;; Auto close brackets
(electric-pair-mode 1)

;; When you select text and then type or paste/yank - delete the selected text
(delete-selection-mode 1)

;; Turn off GUI's tool bar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Default to making frame as large as possible
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Turn off the alarm for some functions
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit
                        mwheel-scroll my-scroll-up my-scroll-down down up next-line previous-line backward-char forward-char))
          (ding))))

;; Show backtrace for errors
;;(setq debug-on-error t)

;; Use spaces for indentation and set default width to 4
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Useful settings for code files
(add-hook 'prog-mode-hook
          (lambda ()
            ;; Truncate lines insted of wrapping
            (set-default 'truncate-lines t)
            ;; Delete trailing whitespace & end files with a newline
            (add-to-list 'write-file-functions 'delete-trailing-whitespace)
            (setq require-final-newline t)
            ;; Turn line numbering on
            (display-line-numbers-mode)
            ;;(linum-mode 1)
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
;; (setq linum-format
;;       (lambda(line)
;;         (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
;;                (propertize (format (format (if (display-graphic-p) " %%%dd" "%%%dd ") w) line) 'face 'linum))))

;(global-linum-mode 1)
;(setq linum-format "%d ")

;; Turn on multiple cursors
(require 'multiple-cursors)

;; --------- ;;
;; Scrolling ;;
;; --------- ;;

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


;; ------------------ ;;
;; Keyboard Shortcuts ;;
;; ------------------ ;;

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
(global-set-key (kbd "<f12> c") 'osx-pbcopy)
(global-set-key (kbd "<f12> x") 'osx-pbcut)

;; Move a line up / down
;; https://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; Multiple-Cursors Key Bindings
;; https://github.com/magnars/multiple-cursors.el
(global-set-key (kbd "C-S-<down>") 'mc/mark-next-like-this)
(global-set-key (kbd "C-S-<up>") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this-word)
(global-set-key (kbd "M-p") 'mc/mark-previous-word-like-this)
(global-set-key (kbd "M-l") 'mc/edit-lines)

;; Similar to what I had before
(global-set-key (kbd "M-U") 'downcase-word)

;; Setup these bindings in iTerm/kitty to get GUI commands
;; The <f12> key represents the command key in the GUI
(global-set-key (kbd "<f12> k") 'kill-this-buffer)
(global-set-key (kbd "<f12> z") 'undo)
(global-set-key (kbd "<f12> l") 'goto-line)


(defun buffer-new ()
  "Creates a new Untitled buffer with the same major mode as the current one."
  (interactive)
  (let
      ((new-buffer (generate-new-buffer "Untitled"))
       (mode major-mode))
    (switch-to-buffer new-buffer)
    (funcall mode)
    (setq buffer-offer-save t)
    (make-local-variable 'kill-buffer-query-functions)
    (add-to-list 'kill-buffer-query-functions 'buffer-prompt-when-modified)
    new-buffer)
  )

(defun buffer-prompt-when-modified ()
  "A prompt to check if should kill buffer without saving. This is meant to be
used in the \"kill-buffer-query-functions\" list for non-file-visiting.
(Added for \"buffer-new\" function)"
  (if (buffer-modified-p)
      (y-or-n-p "Buffer modified; kill anyway? ")
    t)
  )

(global-set-key (kbd "s-n") 'buffer-new)
(global-set-key (kbd "s-N") '(lambda () (interactive) (make-frame) (buffer-new)))

;; Setup these bindings in iTerm/kitty so behaves like the GUI.
(global-set-key (kbd "<f12> n") 'buffer-new)



;; Nice way to rename a file
;; (From: http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/)
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))
(global-set-key (kbd "C-x C-r")  'rename-file-and-buffer)
