;;; ruby.el --- Ruby editing configuration  -*- lexical-binding: t -*-
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


;; Every Ruby project can pin its own Ruby, and therefore its own ruby-lsp.
;; The environment must be activated BEFORE eglot launches a server: otherwise
;; eglot resolves "ruby-lsp" off the ambient PATH and could start the wrong one.
(defun my-ruby-activate-project-env ()
  "Activate the current buffer's Ruby environment.
Prefers shadowenv, falls back to chruby.  Returns a symbol naming
whichever was used, or nil, which is handy when debugging."
  (cond
   ;; shadowenv sets buffer-local `process-environment' and `exec-path', so it
   ;; cannot leak into buffers belonging to other projects.
   ((and (fboundp 'shadowenv-mode)
         ;; bound-and-true-p: the autoload cookie makes `shadowenv-mode'
         ;; fboundp before shadowenv.el is actually loaded, at which point
         ;; `shadowenv-binary-location' is still void.
         (or (bound-and-true-p shadowenv-binary-location)
             (executable-find "shadowenv"))
         (locate-dominating-file default-directory ".shadowenv.d"))
    (shadowenv-mode 1)
    'shadowenv)
   ;; chruby mutates the *global* environment -- that is simply how chruby.el
   ;; works -- so with several Ruby projects open the last one opened wins.
   ;; Fine on single-project machines, which is where this branch is used.
   ((and (fboundp 'chruby-use-corresponding)
         (locate-dominating-file default-directory ".ruby-version"))
    (chruby-use-corresponding)
    'chruby)))

(add-hook 'ruby-base-mode-hook
          (lambda ()
            ;; CamelCase aware editing operations
            (subword-mode 1)
            (inf-ruby-minor-mode 1)
            ;; Activate the project's Ruby first, so that eglot gets the right ruby-lsp.
            (my-ruby-activate-project-env)
            ;; Start ruby-lsp, but only for files that belong to a project.
            (when (project-current)
              (eglot-ensure))
            ))

;; Disable adding magic encoding comments to UTF-8 files
(setq ruby-insert-encoding-magic-comment nil)
;;(setq enh-ruby-add-encoding-comment-on-save 0) ;; If I ever add enh-ruby

;; Let's not indent everything so deep
(setq ruby-align-to-stmt-keywords t)


;; Configure chruby
(require 'chruby)
(advice-add 'inf-ruby-console-auto :before #'chruby-use-corresponding)

;; (with-eval-after-load 'eglot
;;   ;; Pin ruby-lsp. Eglot's stock Ruby entry tries solargraph first and only
;;   ;; falls back to ruby-lsp, so installing solargraph anywhere would silently
;;   ;; change which server starts.
;;   (add-to-list 'eglot-server-programs
;;                '((ruby-mode ruby-ts-mode) . ("ruby-lsp")))
;;
;;   ;; ruby-lsp runs `bundle install' for its composed bundle at startup. In a
;;   ;; large monorepo that took ~58s on this machine even with everything warm,
;;   ;; well past eglot's 30s default, after which eglot reports the server as
;;   ;; dead. The first ever run (cold gem cache) is slower still.
;;   (setq eglot-connect-timeout 180))

;; Allow for pry / byebug break points when running specs
;; When you've hit the breakpoint, hit C-x C-q to enable inf-ruby
(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(defun rspec-run-in-terminal ()
  "Runs the current file in the test terminal window"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let* ((pr (project-current t))
             (root (project-root pr))
             (relative_path (file-relative-name filename root)))
        (terminal-run-command-in-custom-window (s-concat "be rspec '" (s-replace "'" "'\"'\"'" relative_path) "'") (s-concat (project-name pr) ": RSpec") root)))))

(defun rspec-run-at-point-in-terminal ()
  "Runs the current file in the test terminal window"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let* ((pr (project-current t))
             (root (project-root pr))
             (relative_path (file-relative-name filename root)))
        (terminal-run-command-in-custom-window (s-concat "be rspec '" (s-replace "'" "'\"'\"'" relative_path) ":" (format-mode-line "%l") "'") (s-concat (project-name pr) ": RSpec") root)))))
