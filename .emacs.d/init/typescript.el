;;; typescript.el --- TypeScript editing configuration  -*- lexical-binding: t -*-

(setq typescript-ts-indent-offset js-indent-level)

;; --- Project-local tooling ---
;; JS/TS tooling is installed per project, into node_modules/.bin, which is not
;; on PATH.  Putting it on a *buffer-local* `exec-path' lets eglot resolve the
;; project's own server, and means one project's node_modules cannot leak into
;; buffers belonging to another.  (Apheleia does not need this; its bundled
;; apheleia-npx script does the same lookup itself, and also handles yarn pnp.)
(defun my-node-modules-bin-dirs ()
  "Every existing node_modules/.bin at or above `default-directory'.
Nearest first, which is the order npm itself resolves binaries in, so
workspace packages shadow the repo root."
  (let ((dirs nil)
        (dir default-directory))
    (while dir
      (let ((found (locate-dominating-file dir "node_modules")))
        (if (null found)
            (setq dir nil)
          (let ((bin (expand-file-name "node_modules/.bin" found)))
            (when (file-accessible-directory-p bin)
              (push bin dirs)))
          ;; Keep walking up from the parent of FOUND, so nested workspaces stack.
          (let ((up (file-name-directory (directory-file-name found))))
            (setq dir (unless (equal up found) up))))))
    (nreverse dirs)))

(defun my-node-activate-project-env ()
  "Prepend this project's node_modules/.bin to the buffer's `exec-path'."
  (when-let* ((bins (my-node-modules-bin-dirs)))
    (setq-local exec-path (append bins exec-path))))

;; --- Format on save ---
;; Prettier, project-local if there is one, else PATH, else silently skipped.
;; `typescript-ts-base-mode' covers both typescript-ts-mode and tsx-ts-mode.
(use-package apheleia
  :custom
  ;; Defer to the repo's prettier config for the indent width, so that saving
  ;; here produces the same bytes as prettier does in CI.
  (apheleia-formatters-respect-indent-level nil)
  :hook (typescript-ts-base-mode . apheleia-mode))

;; --- LSP ---
;; Gate `eglot-ensure' on a server being resolvable: otherwise eglot signals
;; "None of ... are valid executables" every time a TypeScript file is visited.
;; `executable-find' consults the buffer-local `exec-path' set just above, so a
;; typescript-language-server in node_modules counts.
;;
;; NOTE: Emacs 31.1's stock entry for these modes offers ("rass ts") first --
;; one string containing a space, so `executable-find' can never match it (the
;; python entry right above it correctly uses ("rass" "python")).  Eglot skips
;; it silently and lands on typescript-language-server, so this works today,
;; but installing the rass multiplexer would have no effect until that entry is
;; fixed or overridden here.
(add-hook 'typescript-ts-base-mode-hook
          (lambda ()
            (my-node-activate-project-env)
            ;; Only for files that belong to a project, as with ruby-lsp.
            (when (and (project-current)
                       (executable-find "typescript-language-server"))
              (eglot-ensure))))
