;; Format the mode-line at the bottom
(defvar my-mode-line-position-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      (lambda (e)
        (interactive "e")
        (call-interactively 'goto-line)))
    (purecopy map)))

(defvar my-mode-line-position
  (list (propertize
         "L:%l C:%c"
         'local-map my-mode-line-position-map
         'mouse-face 'mode-line-highlight
         'help-echo "Line number and Column number\n\
mouse-1: Go to line"))
  "Mode line construct for displaying whether current buffer is modified.")
(put 'my-mode-line-position 'risky-local-variable t)

(defvar my-mode-line-major-mode
  (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out"))
    (list (propertize "%[" 'help-echo recursive-edit-help-echo)
          "["
          `(:propertize ("" mode-name)
                        help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                        mouse-face mode-line-highlight
                        local-map ,mode-line-major-mode-keymap)
          '("" mode-line-process)
          "]"
          (propertize "%]" 'help-echo recursive-edit-help-echo)
          " "))
  "Mode line construct for displaying major mode.")
(put 'my-mode-line-major-mode 'risky-local-variable t)

;; (defvar my-mode-line-minor-modes
;;   (list
;;    `(:propertize ("" minor-mode-alist)
;;                  help-echo "Minor modes"))
;;   "Mode line construct for displaying major and minor modes.")

(defvar my-mode-line-minor-modes
  '(:eval (my-minor-modes-for-mode-line))
  "Mode line construct for displaying major and minor modes.")
(put 'my-mode-line-minor-modes 'risky-local-variable t)


;; Allow for right-aligned items
;; https://emacs.stackexchange.com/a/37270/21578
(defun simple-mode-line-render (left flex-left flex-right right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((left-size (length (format-mode-line left)))
         (right-size (length (format-mode-line right)))
         (available-width (- (window-total-width) left-size right-size))
         (lflex-size (length (format-mode-line flex-left)))
         (rflex-size (length (format-mode-line flex-right)))
         (flex-size (+ lflex-size rflex-size))
         (sep (list (propertize "…"))))
    (cond
     ((>= available-width flex-size)
      (append left flex-left (list (format (format "%%%ds" (- available-width flex-size)) "")) flex-right right))

     ((> available-width lflex-size)
      (append left flex-left (list (format (format "%%%ds" (- available-width lflex-size 1)) "")) sep right))

     ((> available-width rflex-size)
      (append left sep (list (format (format "%%%ds" (- available-width rflex-size)) "")) flex-right right))

     (t (append left sep right)))
    ))


;; Adapted from rich-minority rm-format-mode-line-entry
;; https://github.com/Malabarba/rich-minority/blob/master/rich-minority.el
(defun my-minor-modes-with-formated (entry)
  "Takes an entry of `minor-mode-alist' and returns the mode-line string if it's \
  for the minor modes I'm interested in and the mode line string is not empty."
  (let ((mode-symbol (car entry))
        (mode-string (format-mode-line entry)))
    (unless (or
             (string= mode-string "")
             (not (eq mode-symbol 'multiple-cursors-mode)))
      mode-string)))

(defun my-minor-modes-for-mode-line ()
  "Let's limit this to the minor modes I actually want to see in my mode-line"
  (delq nil (mapcar #'my-minor-modes-with-formated minor-mode-alist)))





(setq-default
 mode-line-format
 '((:eval
    (simple-mode-line-render
     ;; left
     (list
      "%e"
      'mode-line-front-space
      'mode-line-mule-info
      ;; 'mode-line-client  ;; don't care about identifying frames
      'mode-line-modified
      ;; 'mode-line-remote  ;; only work on local files
      ;; 'mode-line-frame-identification ;; more frame stuff
      " "
      'mode-line-buffer-identification
      "   "
      ;; 'mode-line-end-spaces
      )
     ;; flex left
     (list
      ""
      '(vc-mode vc-mode))
     ;; flex right
     (list
      ;; 'mode-line-misc-info
      ;; mode-line-modes
      " "
      'my-mode-line-minor-modes)
     ;; right
     (list
      " "
      'my-mode-line-major-mode
      ;; 'mode-line-position
      " "
      'my-mode-line-position
      " ")))))
