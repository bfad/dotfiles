;;; packages.el --- Package archives and use-package bootstrap  -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Install anything a `use-package` form declares but that is missing.
;; (NOTE: Built-in packages need `:ensure nil` so no archive lookup is attempted for them.)
(setq use-package-always-ensure t)

(use-package magit :defer t)
(use-package visual-fill-column :defer t)
