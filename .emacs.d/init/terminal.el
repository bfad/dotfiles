(defun terminal-run-command-in-custom-window (command window_prefix window_title &optional new_window_dir window_size)
  "Runs a command in the terminal"
  (unless new_window_dir (setq new_window_dir "~"))
  (setq command (s-replace "\"" "\\\"" command))
  (setq window_prefix (s-replace "\"" "\\\"" window_prefix))
  (setq window_title (s-replace "\"" "\\\"" window_title))
  (setq new_window_dir (s-replace "\"" "\\\"" (or new_window_dir "")))
  (setq window_size (s-replace "\"" "\\\"" (or window_size "")))
  (do-applescript (s-replace-all `(("$$COMAND$$" . ,command) ("$$WINDOW_PREFIX$$" . ,window_prefix) ("$$WINDOW_TITLE$$" . ,window_title) ("$$STARTING_DIR$$" . ,new_window_dir) ("$$WINDOW_SIZE$$" . ,window_size)) "\
property IDE : script \"IDE\"

tell IDE to runInTerminal({¬
  prefix: \"📼$$WINDOW_PREFIX$$\",¬
  name: \"$$WINDOW_TITLE$$\",¬
  initDir: \"$$STARTING_DIR$$\",¬
  command: \"$$COMAND$$\",¬
  size: \"$$WINDOW_SIZE$$\"¬
})
")))

(defun terminal-goto-or-open-window-for-current-project (&optional size title)
  "Goes to or launches terminal tab for project optionally with specified size / title"
  (interactive)
  (setq size (s-replace "\"" "\\\"" (or size "")))
  (setq prefix (s-replace "\"" "\\\"" (projectile-project-name)))
  (setq title (s-replace "\"" "\\\"" (or title (car (vc-git-branches)) "")))
  (setq new_window_dir (projectile-project-root))
  (do-applescript (s-replace-all `(("$$PREFIX$$" . ,prefix) ("$$TITLE$$" . ,title) ("$$SIZE$$" . ,size) ("$$STARTING_DIR$$" . ,new_window_dir)) "\
tell script \"IDE\" to runInTerminal({¬
  prefix: \"📼$$PREFIX$$\",¬
  name: \"$$TITLE$$\",¬
  initDir: \"$$STARTING_DIR$$\",¬
  size: \"$$SIZE$$\"¬
})
")))

(global-set-key (kbd "C-s-t") 'terminal-goto-or-open-window-for-current-project)
