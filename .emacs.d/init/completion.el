;;; completion.el --- Completion system  -*- lexical-binding: t -*-
;; Vertico (minibuffer) and Corfu (in-buffer) are counterparts, both built on
;; the stock completion facilities: orderless supplies the matching style,
;; marginalia the annotations, cape the extra completion-at-point sources.
;;
;; Commands that consume completing-read are in init/consult.el; acting on
;; candidates is in init/embark.el.

;; ---------- ;;
;; Minibuffer ;;
;; ---------- ;;

(vertico-mode 1)
(setq vertico-cycle t)
(setq vertico-count 15)

;; Path-aware RET and DEL inside file prompts.
(require 'vertico-directory)
(define-key vertico-map (kbd "RET")   #'vertico-directory-enter)
(define-key vertico-map (kbd "DEL")   #'vertico-directory-delete-char)
(define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
(add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

;; Keep s-g moving down the candidate list.
(define-key vertico-map (kbd "s-g") #'vertico-next)

;; -------- ;;
;; Matching ;;
;; -------- ;;

;; Orderless has to be loaded to use `orderless-define-completion-style` below.
(require 'orderless)

;; Fuzzy-matching
(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq orderless-matching-styles
      '(orderless-literal orderless-regexp orderless-flex))

;; A flex-free variant for callers that want steadier candidate ordering.
;; init/consult.el uses it for consult-line.
(orderless-define-completion-style my-orderless-literal
  "Orderless with flex matching left out."
  (orderless-matching-styles '(orderless-literal orderless-regexp)))

;; Per-category tuning.  Packages add their own entries from their own files.
(setq completion-category-overrides
      '((file (styles partial-completion orderless))))

;; ----------- ;;
;; Annotations ;;
;; ----------- ;;

;; Docstrings for commands, major mode and size for files.
(marginalia-mode 1)

;; --------- ;;
;; In-buffer ;;
;; --------- ;;

(global-corfu-mode 1)
;; corfu-mode requires the bundled corfu-auto.el itself once this is set;
;; corfu-auto-prefix and corfu-auto-delay are defined there.
(setq corfu-auto t)
(setq corfu-auto-prefix 1)
(setq corfu-auto-delay 0.3)
(setq corfu-cycle t)

;; Documentation for the selected candidate, beside the popup.
(with-eval-after-load 'corfu
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode 1))

;; Cape supplies the generic completions.
;; Listed in the reverse order they run as add-hook prepends.
(add-hook 'completion-at-point-functions #'cape-dabbrev)
(add-hook 'completion-at-point-functions #'cape-file)
