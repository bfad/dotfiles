;; Configure Ruby Code Editing

;; Rake files are ruby, too, as are gemspecs, rackup files, and gemfiles.
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.cap\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Podfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.podspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Puppetfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Appraisals\\'" . ruby-mode))

;; We never want to edit Rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(define-key 'help-command (kbd "R") 'yari)

(add-hook 'ruby-mode-hook
          (lambda ()
            ;; CamelCase aware editing operations
            (subword-mode 1)
            (inf-ruby-minor-mode 1)
            (robe-mode 1)
            ))

;; Disable adding magic encoding comments to UTF-8 files
(setq ruby-insert-encoding-magic-comment nil)
;;(setq enh-ruby-add-encoding-comment-on-save 0) ;; If I ever add enh-ruby

;; Let's not indent everything so deep
(setq ruby-align-to-stmt-keywords t)

;; Configure robe company support
(eval-after-load 'company
  '(push 'company-robe company-backends))

;; Configure chruby
(require 'chruby)
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (chruby-use-corresponding))

;; Allow for pry / byebug break points when running specs
;; When you've hit the breakpoint, hit C-x C-q to enable inf-ruby
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(defun rspec-run-in-terminal ()
  "Runs the current file in the test terminal window"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((relative_path (file-relative-name filename (projectile-project-root))))
        (terminal-run-command-in-custom-window (s-concat "be rspec '" (s-replace "'" "'\"'\"'" relative_path) "'") (s-concat (projectile-project-name) ": RSpec") (projectile-project-root))))))

(defun rspec-run-at-point-in-terminal ()
  "Runs the current file in the test terminal window"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((relative_path (file-relative-name filename (projectile-project-root))))
        (terminal-run-command-in-custom-window (s-concat "be rspec '" (s-replace "'" "'\"'\"'" relative_path) ":" (format-mode-line "%l") "'") (s-concat (projectile-project-name) ": RSpec") (projectile-project-root))))))
