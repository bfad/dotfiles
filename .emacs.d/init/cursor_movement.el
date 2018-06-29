(defun smarter-move-beginning-of-line (arg)
    "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there.
FotBar
foo_bar
Taken from:
http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/"
    (interactive "^p")
    (setq arg (or arg 1))

    ;; Move lines first
    (when (/= arg 1)
      (let ((line-move-visual nil))
        (forward-line (1- arg))))

    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1))))

(defun backward-symbol (&optional arg)
  "Move backward until encountering the beginning of a symbol.
With argument, do this that many times.
Taken from:
https://lists.gnu.org/archive/html/emacs-devel/2007-01/msg01025.html"
  (interactive "^p")
  (forward-symbol (- (or arg 1))))


;; ----------------------- ;;
;; Functions for Selection ;;
;; ----------------------- ;;
;; Modified from http://emacs.stackexchange.com/a/22166/93
(defun my-mark-to-end-of-line (&optional arg)
  "Uses shift selection to select to the end of the current line.
When there is an existing shift selection, extends the selection."
  (interactive "p")
  (let ((oldval (or (cdr-safe transient-mark-mode) transient-mark-mode))
        (beg (and mark-active (mark-marker))))
    (unless beg
      ;;(end-of-line) ;;(beginning-of-line))
      (setq beg (point-marker)))
    ;;(if backwards (end-of-line (- 1 arg)) (beginning-of-line (+ 1 arg)))
    (end-of-line arg)
    (unless mark-active
      (push-mark beg nil t))
    (setq transient-mark-mode (cons 'only oldval))))

;; Modified from http://emacs.stackexchange.com/a/22166/93
(defun my-mark-to-smarter-beginning-of-line (&optional arg)
  "Uses shift selection to select the beginning of the current line.
When there is an existing shift selection, extends the selection."
  (interactive "p")
  (let ((oldval (or (cdr-safe transient-mark-mode) transient-mark-mode))
        (beg (and mark-active (mark-marker))))
    (unless beg
      (setq beg (point-marker)))
    (smarter-move-beginning-of-line arg)
    (unless mark-active
      (push-mark beg nil t))
    (setq transient-mark-mode (cons 'only oldval))))

;; Modified from http://emacs.stackexchange.com/a/22166/93
(defun my-mark-to-end-of-buffer (&optional arg)
  "Uses shift selection to select to the end of the buffer.
When there is an existing shift selection, extends the selection."
  (interactive "p")
  (let ((oldval (or (cdr-safe transient-mark-mode) transient-mark-mode))
        (beg (and mark-active (mark-marker))))
    (unless beg
      ;;(end-of-line) ;;(beginning-of-line))
      (setq beg (point-marker)))
    ;;(if backwards (end-of-line (- 1 arg)) (beginning-of-line (+ 1 arg)))
    ;;(end-of-buffer arg)
    (goto-char (point-max))
    (unless mark-active
      (push-mark beg nil t))
    (setq transient-mark-mode (cons 'only oldval))))

;; Modified from http://emacs.stackexchange.com/a/22166/93
(defun my-mark-to-beginning-of-buffer (&optional arg)
  "Uses shift selection to select the beginning of the buffer.
When there is an existing shift selection, extends the selection."
  (interactive "p")
  (let ((oldval (or (cdr-safe transient-mark-mode) transient-mark-mode))
        (beg (and mark-active (mark-marker))))
    (unless beg
      (setq beg (point-marker)))
    ;;(beginning-of-buffer arg)
    (goto-char (point-min))
    (unless mark-active
      (push-mark beg nil t))
    (setq transient-mark-mode (cons 'only oldval))))

(defun my-kill-to-beginning-of-line ()
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))



;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)


;; Move by symbol (approximates option-arrow in OS X)
(global-set-key (kbd "M-f") 'forward-symbol)
(global-set-key (kbd "M-<right>") 'forward-symbol)

(global-set-key (kbd "M-b") 'backward-symbol)
(global-set-key (kbd "M-<left>") 'backward-symbol)


;; Moving by (sub)word
(global-set-key (kbd "C-M-f") 'subword-forward)
(global-set-key (kbd "C-M-<right>") 'subword-forward)
(global-set-key (kbd "<C-right>") 'subword-forward)

(global-set-key (kbd "C-M-b") 'subword-backward)
(global-set-key (kbd "C-M-<left>") 'subword-backward)
(global-set-key (kbd "<C-left>") 'subword-backward)


(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "s-<left>") 'smarter-move-beginning-of-line)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)


;; Deleting
(global-set-key (kbd "s-<backspace>") 'my-kill-to-beginning-of-line)
;; For iTerm s-<backspace> is mapped to C-S-M-delete
(global-set-key (kbd "C-S-M-<delete>") 'my-kill-to-beginning-of-line)
;(lambda ()
;                                        (interactive)
;                                        (kill-line 0)
;                                        (indent-according-to-mode)))


;; Selection
;; found these escape sequences by pressing C-h b
;; M-[ 1 ; 8 A     <C-M-S-up>
;; M-[ 1 ; 8 B     <C-M-S-down>
;; M-[ 1 ; 8 C     <C-M-S-right>
;; M-[ 1 ; 8 D     <C-M-S-left>
;; Setup iTerm to send the sequences when pressing s-S-arrow
(global-set-key (kbd "C-M-S-<right>") 'my-mark-to-end-of-line)
(global-set-key (kbd "C-M-S-<left>") 'my-mark-to-smarter-beginning-of-line)
(global-set-key (kbd "C-M-S-<up>") 'my-mark-to-beginning-of-buffer)
(global-set-key (kbd "C-M-S-<down>") 'my-mark-to-end-of-buffer)

;; Other keyboard setup
(setq mac-right-option-modifier nil)
