;;; .yas-setup.el --- yasnippet helpers for prog-mode  -*- lexical-binding: t -*-
(defun yas-with-comment (str)
  (format "%s%s%s" comment-start str comment-end))
