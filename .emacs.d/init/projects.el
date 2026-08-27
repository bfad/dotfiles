;;; projects.el --- Projects via built-in project.el  -*- lexical-binding: t -*-
;; Named projects.el rather than project.el so it can never shadow the
;; built-in library if init/ ends up on `load-path'.
;;
;; Loads after init/completion.el: my-project-find-file below let-binds
;; `orderless-style-dispatchers', so orderless has to be loaded already.

;; project.el recognises VCS roots only.  Add the common build files so a
;; directory that is a project but not a repository still counts.
(setq project-vc-extra-root-markers '("Gemfile" "package.json" "Makefile"))

;; The built-in prefix is C-x p; keep C-c p working too.
(global-set-key (kbd "C-c p") project-prefix-map)

;; Ignore a trailing :LINE or :LINE:COL when matching.  Returning a string
;; tells orderless to replace the component and keep dispatching.  This is not
;; global orderless configuration -- my-project-find-file let-binds it -- so it
;; lives here with its only caller.
(defun my-orderless-ignore-line-column (component _index _total)
  "Strip a trailing :LINE or :LINE:COL from COMPONENT."
  (save-match-data
    (when (string-match "\\`\\(.+?\\)\\(?::[[:digit:]]*\\)\\{1,2\\}\\'" component)
      (match-string 1 component))))

(defun my-project-find-file ()
  "Find a file in the current project, allowing a trailing :LINE:COL.
Typing app/models/user.rb:42:10 opens that file at line 42, column 10, so a
path copied from a stack trace or a grep hit can be pasted straight in."
  (interactive)
  (let* ((pr (project-current t))
         (root (project-root pr))
         (files (mapcar (lambda (f) (file-relative-name f root))
                        (project-files pr)))
         (orderless-style-dispatchers
          (cons #'my-orderless-ignore-line-column orderless-style-dispatchers))
         typed)
    ;; completing-read hands back the chosen candidate, which has already had
    ;; the suffix stripped, so record what was actually typed to recover it.
    (let ((file (minibuffer-with-setup-hook
                    (lambda ()
                      (add-hook 'post-command-hook
                                (lambda ()
                                  (setq typed (minibuffer-contents-no-properties)))
                                nil t))
                  (completing-read "Find file: " files nil t))))
      (when file
        (find-file (expand-file-name file root))
        (save-match-data
          (when (and typed
                     (string-match ":\\([[:digit:]]+\\)\\(?::\\([[:digit:]]+\\)\\)?\\'" typed))
            (let ((line (match-string 1 typed))
                  (col  (match-string 2 typed)))
              ;; Grep hits and stack traces count lines and columns from 1,
              ;; but Emacs columns start at 0, so the column needs one
              ;; subtracted.  Clamp both: move-to-column signals on a
              ;; negative argument, so a pasted :0 would otherwise error.
              (goto-char (point-min))
              (forward-line (max 0 (1- (string-to-number line))))
              (when col
                (move-to-column (max 0 (1- (string-to-number col))))))))))))

(global-set-key (kbd "s-t") #'my-project-find-file)
(global-set-key (kbd "<f12> t") #'my-project-find-file)

;; Entries are (COMMAND LABEL &optional KEY); KEY is required for commands
;; that are not in `project-prefix-map'.  my-neotree-project-action is defined
;; in init.el, next to the rest of the neotree configuration; the reference is
;; a quoted symbol so it resolves when the menu runs, not when this loads.
(setq project-switch-commands
      '((my-neotree-project-action "Find file (neotree)" ?f)
        (project-find-regexp "Find regexp")
        (project-find-dir "Find directory")
        (project-dired "Dired")
        (project-vc-dir "VC-Dir")
        (project-eshell "Eshell")))
