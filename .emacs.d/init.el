;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Start with adding packages
(load "~/.emacs.d/init/packages")
(load "~/.emacs.d/init/cursor_movement")

;; Put anything relating to basic editor behavior and navigation in here
(load "~/.emacs.d/init/editor")
(load "~/.emacs.d/init/appearance")
(load "~/.emacs.d/init/terminal")

;; Emacs desktop (saves state to reload when reopening)
;; When running an emacs daemon, start it with `--no-desktop` option so it won't
;; load the desktop frames itself. To load saved desktop in GUI client, run:
;; `emacsclient -n -c -e '(progn (desktop-save-mode 1) (desktop-read))`
(setq desktop-restore-in-current-display t)
(desktop-save-mode 1)

;; Setup auto-complete
;;(require 'auto-complete-config)
;;(ac-config-default)
;; Start after 1 character
;;(setq ac-auto-start 1)
;; Shorter delay
;;(setq ac-auto-show-menu 0.2)
;;(setq ac-use-fuzzy t)

;; setup company-flx
(with-eval-after-load 'company
  (company-flx-mode +1))


;; Configure ido
(require 'flx-ido)
(require 'ido-vertical-mode)

;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(ido-vertical-mode 1)

;Colors Make it look like @abo-abo's: http://oremacs.com/2015/02/09/ido-vertical/
;(setq ido-use-faces t)
;(set-face-attribute 'ido-vertical-first-match-face nil
;                    :background "#e5b7c0")
;(set-face-attribute 'ido-vertical-only-match-face nil
;                    :background "#e52b50"
;                    :foreground "white")
;(set-face-attribute 'ido-vertical-match-face nil
;                    :foreground "#b00000")
;(ido-vertical-mode 1)


;; Configure projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(global-set-key (kbd "s-t") 'projectile-find-file)
(global-set-key (kbd "<f12> t") 'projectile-find-file)

;; Configure Origami for code folding
(require 'origami)
(global-origami-mode)

;; Configure neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-window-fixed-size nil)

;; Adapted from neotree-projectile-action function in neotree project
(defun my-neotree-projectile-action ()
  "Integration with `Projectile'.
Usage:
    (setq projectile-switch-project-action 'my-neotree-projectile-action).
When running `projectile-switch-project' (C-c p p), `neotree' will change root
automatically but projectile-find-file will still be called."
  ;;(interactive)
  (cond
   ((fboundp 'projectile-project-root)
    (neotree-dir (projectile-project-root))
    (neotree-hide)
    (projectile-find-file))
   (t
    (error "Projectile is not available"))))
(setq projectile-switch-project-action 'my-neotree-projectile-action)

;; Configure RipGrep via deadgrep
(global-set-key (kbd "C-c s") #'deadgrep)

;; Configure hydra
(require 'hydra)
(load "~/.emacs.d/init/hydra/modes")
(load "~/.emacs.d/init/hydra/window_management")
(load "~/.emacs.d/init/hydra/movement")
(load "~/.emacs.d/init/hydra/cursors")
(load "~/.emacs.d/init/hydra/folding")

;; Configure key-chord for use with hydra
(require 'key-chord)
(key-chord-mode t)
(key-chord-define-global ";h" 'hydra-modes/body)
(key-chord-define-global ";w" 'hydra-window/body)
(key-chord-define-global ";m" 'hydra-movement/body)
(key-chord-define-global ";c" 'hydra-cursors/body)
(key-chord-define-global ";f" 'hydra-folding/body)

;; Configure scss-mode
(setq scss-compile-at-save nil)

;; Configure yasnippet
(require 'yasnippet)
(yas-reload-all)

;; Configure JS indentation level
(setq js-indent-level 2)
(setq coffee-tab-width 2)

;; Setup git config
(require 'git-commit)
(add-hook 'git-commit-mode-hook (lambda () (setq-default fill-column 72)))

;; Configure coffee-mode
;; Configure coffee-mode
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "red3" "green3" "yellow3" "blue2" "magenta3" "cyan3" "gray90"])
 '(cursor-type (quote bar))
 '(package-selected-packages
   (quote
    (string-inflection yari slim-mode scss-mode rinari origami key-chord ido-vertical-mode fuzzy flx-ido company-web company-inf-ruby company-flx coffee-mode chruby auto-complete alchemist)))
 '(safe-local-variable-values
   (quote
    ((eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))))))

;; Configure css-mode
;; TODO: Check this works with scss and sass
(add-hook 'css-mode-hook
          (lambda ()
            (rainbow-mode 1)
            ))

(setq-default elm-indent-offset 2)

;; Load ruby settings
(load "~/.emacs.d/init/ruby")

;; Configure YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Configure dockerfile-mode
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.dockerfile\\'" . dockerfile-mode))

;; Elm mode
(require 'elm-mode)

;; Alchemist for Elixir
(require 'alchemist)

;; Pug templates
(require 'pug-mode)

;; Web Mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))

;; Set Emacs variable exec-path if launching MacOS GUI
;; Useful for MacOS GUI and using ag.el when ag is in /usr/local/bin
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
