;;; treesit-setup.el --- Tree-sitter major modes  -*- lexical-binding: t -*-

;; Emacs 31 ships `treesit-major-mode-remap-alist' pre-populated with 26
;; entries (ruby-mode -> ruby-ts-mode, yaml-mode -> yaml-ts-mode, ...).
;; `treesit-enabled-modes' is the switch that activates them.
;;
;; IMPORTANT: `treesit-enabled-modes' has a :set function whose job is to
;; populate `major-mode-remap-alist'.  A plain `setq' bypasses the setter and
;; silently does nothing -- it MUST be set via `customize-set-variable'.

(require 'treesit)
(require 'seq)

;; Recipes for `treesit-install-language-grammar'.  Grammars are cloned and
;; compiled from source into ~/.emacs.d/tree-sitter/, which is gitignored
;; (machine-specific compiled binaries, not configuration).
;;
;; Recipe format: (LANG . (URL [KEYWORD VALUE]...))
;; Keywords are used below rather than the positional (URL REVISION ...) form,
;; which treesit.el marks as "Old positional convention for
;; backward-compatibility".  Available: :revision :source-dir :cc :c++
;; :commit :copy-queries
;;
;; Every entry is pinned to a release tag.  Unpinned, the installer clones
;; whatever the default branch happens to be, with no checksum -- so the
;; grammar you get is not reproducible across machines or over time.
;;
;; ABI: Emacs 31.1's bundled tree-sitter supports language versions 13-15
;; (`treesit-library-abi-version' => 15, minimum 15 => 13).  Each tag below
;; was checked against that range; a grammar built with a newer ABI will not
;; load.  Verify before bumping a tag:
;;
;;   curl -sL https://raw.githubusercontent.com/<repo>/<tag>/src/parser.c \
;;     | grep -m1 LANGUAGE_VERSION
;;
;; Tags resolve to these commits (as of pinning).  Tags are mutable in
;; principle; for strict immutability move each SHA into the COMMIT slot,
;; which takes precedence over REVISION:
;;
;;   ruby       v0.23.1  71bd32fb7607035768799732addba884a37a6210  ABI 14
;;   yaml       v0.7.2   7708026449bed86239b1cd5bce6e3c34dbca6415  ABI 14
;;   dockerfile v0.2.0   868e44ce378deb68aac902a9db68ff82d2299dd0  ABI 14
;;   css        v0.25.0  dda5cfc5722c429eaba1c910ca32c2c0c5bb1a3f  ABI 15
;;   javascript v0.25.0  44c892e0be055ac465d5eeddae6d3e194424e7de  ABI 15
;;   jsdoc      v0.23.2  b253abf68a73217b7a52c0ec254f4b6a7bb86665  ABI 14
;;   json       v0.24.8  ee35a6ebefcef0c5c416c0d1ccec7370cfca5a24  ABI 14
;;   bash       v0.25.1  a06c2e4415e9bc0346c6b86d401879ffb44058f7  ABI 15
;;
;; NOTE: js-ts-mode requires two grammars: javascript and jsdoc.
;;
;; NOTE on yaml: this uses the tree-sitter-grammars fork, not ikatyang's
;; original.  ikatyang/tree-sitter-yaml has published no releases at all
;; (newest tag v0.5.0, no release), while the fork is maintained at v0.7.2.
(setq treesit-language-source-alist
      '((ruby       "https://github.com/tree-sitter/tree-sitter-ruby"
                    :revision "v0.23.1")
        (yaml       "https://github.com/tree-sitter-grammars/tree-sitter-yaml"
                    :revision "v0.7.2")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile"
                    :revision "v0.2.0")
        (css        "https://github.com/tree-sitter/tree-sitter-css"
                    :revision "v0.25.0")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                    :revision "v0.25.0")
        (jsdoc      "https://github.com/tree-sitter/tree-sitter-jsdoc"
                    :revision "v0.23.2")
        (json       "https://github.com/tree-sitter/tree-sitter-json"
                    :revision "v0.24.8")
        (bash       "https://github.com/tree-sitter/tree-sitter-bash"
                    :revision "v0.25.1")))

(defvar my-treesit-modes
  '((ruby-ts-mode       ruby)
    (yaml-ts-mode       yaml)
    (dockerfile-ts-mode dockerfile)
    (css-ts-mode        css)
    (js-ts-mode         javascript jsdoc)
    (json-ts-mode       json)
    (bash-ts-mode       bash))
  "Tree-sitter major modes wanted here, mapped to the grammars each requires.
A mode is only enabled when every grammar listed for it is available.")

(defun my-treesit-install-missing-grammars ()
  "Compile and install any grammar in `treesit-language-source-alist' that is
missing.  Clones from upstream and runs a C compiler, so it needs network
access and Xcode Command Line Tools."
  (interactive)
  (let ((missing (seq-remove #'treesit-language-available-p
                             (mapcar #'car treesit-language-source-alist))))
    (if (not missing)
        (message "All tree-sitter grammars already installed")
      (dolist (lang missing)
        (message "Installing tree-sitter grammar: %s" lang)
        (treesit-install-language-grammar lang))
      (message "Installed %d grammar(s). Restart Emacs to pick up the modes."
               (length missing)))))

;; Only enable a tree-sitter mode when its grammar is actually present.
;;
;; This matters: `ruby-ts-mode' calls
;;   (unless (treesit-ensure-installed 'ruby)
;;     (error "Tree-sitter for Ruby isn't available"))
;; so if the grammar is missing and you decline the install prompt, the mode
;; signals an error and the buffer is left in a worse state than plain
;; ruby-mode.  Gating on availability degrades cleanly instead: without a
;; grammar you simply get the classic mode, and each ts-mode switches on by
;; itself once its grammar is installed.
;;
;; Run M-x my-treesit-install-missing-grammars, then restart.
;;
;; Why bother: remapping ruby-mode -> ruby-ts-mode upgrades every Rakefile /
;; Gemfile / .gemspec / Vagrantfile entry in init/ruby.el's auto-mode-alist at
;; once, because the remap is consulted by `set-auto-mode' after
;; auto-mode-alist resolves.  It is also what makes code folding work at all:
;; plain ruby-mode defines no hideshow blocks (measured: hs-hide-all produced
;; 0 folds), while ruby-ts-mode declares a `list' thing covering
;; method/class/module/do/case/if/unless/begin.
(customize-set-variable
 'treesit-enabled-modes
 (mapcar #'car
         (seq-filter (lambda (entry)
                       (seq-every-p #'treesit-language-available-p (cdr entry)))
                     my-treesit-modes)))
