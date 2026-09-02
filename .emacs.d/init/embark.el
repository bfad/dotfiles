;;; embark.el --- Act on completion candidates and things at point  -*- lexical-binding: t -*-
;; A contextual menu: in the minibuffer the target is the current candidate,
;; in a normal buffer it is the region, or the file / symbol / url at point.
;; Offered actions depend on the target's type.

(use-package embark
  :bind (("C-."   . embark-act)
         ("C-;"   . embark-dwim)
         ("C-h B" . embark-bindings)))

;; embark-consult exists only to bridge embark and consult: it previews the
;; candidate under point in collect buffers, and teaches embark to export
;; consult results into a grep-style buffer.
(use-package embark-consult
  :after embark
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))
