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

(use-package vertico
  :custom
  (vertico-cycle t)
  (vertico-count 15)
  :bind (:map vertico-map
              ;; Keep s-g moving down the candidate list.
              ("s-g" . vertico-next))
  :init
  (vertico-mode 1))

;; Path-aware RET and DEL inside file prompts.
(use-package vertico-directory
  :ensure nil    ;; ships as part of vertico package, no need to install
  :after vertico
  :demand t
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; -------- ;;
;; Matching ;;
;; -------- ;;

;; Orderless has to be loaded to use `orderless-define-completion-style` below.
;; It's also needed for `my-project-find-file' in init/projects.el.
(use-package orderless
  :demand t
  :custom
  ;; Fuzzy-matching
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (orderless-matching-styles
   '(orderless-literal orderless-regexp orderless-flex))
  (completion-category-overrides
   '((file (styles partial-completion orderless))))
  :config
  ;; A flex-free variant for callers that want steadier candidate ordering.
  ;; init/consult.el uses it for consult-line.
  (orderless-define-completion-style my-orderless-literal
    "Orderless with flex matching left out."
    (orderless-matching-styles '(orderless-literal orderless-regexp))))

;; ----------- ;;
;; Annotations ;;
;; ----------- ;;

;; Docstrings for commands, major mode and size for files.
(use-package marginalia
  :init
  (marginalia-mode 1))

;; --------- ;;
;; In-buffer ;;
;; --------- ;;

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.3)
  (corfu-cycle t)
  :init
  (global-corfu-mode 1)
  :config
  (require 'corfu-auto)
  ;; Documentation for the selected candidate, beside the popup.
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode 1))

;; Cape supplies the generic completions.
(use-package cape
  :defer t
  :init
  ;; Listed in the reverse order they run as add-hook prepends.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))
