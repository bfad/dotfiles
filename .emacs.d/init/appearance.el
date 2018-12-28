;; Default fonts setup (though terminals control their own fonts)
;; Done through a hook to get it to work with emacs in daemon mode
;; and using emacsclient -c to launch the GUI
;(add-hook 'after-make-frame-functions
;          #'(lambda (&optional frame)
;              (if frame (select-frame frame))
;              ;;(message (format "%s" (display-graphic-p)))
;; Don't like the daemon mode -- too many things get screwed up
(if (display-graphic-p)
    (progn
      (add-to-list 'default-frame-alist '(font . "Inconsolata-14"))
      (set-frame-font "Inconsolata-14" nil t)
      (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)))
              ;;))

(add-to-list 'default-frame-alist '(ns-appearance . dark))
(load-theme 'material_darker t)
