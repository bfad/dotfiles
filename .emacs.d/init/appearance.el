;;; appearance.el --- Fonts and theme  -*- lexical-binding: t -*-
;; Default fonts setup (though terminals control their own fonts)
(add-to-list 'default-frame-alist '(font . "Inconsolata-14"))
(set-frame-font "Inconsolata-14" nil t)
(set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(load-theme 'material_darker t)

;; Cursor defaults
(setq-default cursor-type 'bar)
