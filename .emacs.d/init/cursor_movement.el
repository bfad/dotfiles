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


;; Other keyboard setup
(setq mac-right-option-modifier nil)
