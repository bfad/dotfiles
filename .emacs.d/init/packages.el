;; Load all package managers
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(let ((my-packages (list
                    'magit
                    'git-commit
                    'multiple-cursors
                    'visual-fill-column
                    'origami
                    'key-chord
                    'projectile
                    'flx-ido
                    'ido-vertical-mode
                    'ag
                    'deadgrep
                    'exec-path-from-shell
                    'neotree
                    'hydra
                    'avy
                    ;'auto-complete
                    'company
                    'company-flx
                    'company-inf-ruby
                    'company-web
                    'yasnippet
                    'web-mode
                    'chruby
                    'inf-ruby
                    'yari
                    'rinari
                    'robe
                    'rspec-mode
                    'haml-mode
                    'slim-mode
                    'coffee-mode
                    'scss-mode
                    'sass-mode
                    'markdown-mode
                    'rainbow-mode
                    'elixir-mode
                    'alchemist
                    'dockerfile-mode
                    'yaml-mode
                    'elm-mode
                    'pug-mode
                    'nginx-mode
                    ;'helm
                    ;'helm-projectile
                    ;'helm-ag ;; Can use with ack
                    ;'helm-descbinds
                    ))
      (package-list-refreshed nil))

  ;; If package descriptions have never been downloaded, get them
  (or (file-exists-p package-user-dir)
      (progn (package-refresh-contents) (setq package-list-refreshed t)))
  ;; Activate installed packages
  (package-initialize)

  ;; Ensure packages are installed
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (unless package-list-refreshed
         (package-refresh-contents)
         (setq package-list-refreshed t))
       (package-install package)))
   my-packages)
)
