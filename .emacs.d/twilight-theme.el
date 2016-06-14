;;; twilight-theme.el --- Theme based on Textmate's Twilight theme for faces
;;; Code based on the tango-dark theme code

(deftheme twilight
  "Face colors using the Twilight theme from Textmate.
Basic, Font Lock, Isearch, Gnus, Message, Ediff, Flyspell,
Semantic, and Ansi-Color faces are included.")

(let ((class '((class color) (min-colors 89)))
      ;; Twilight palette colors.
      (gray-1 "#141414") (gray-1.2 "#2F2F2F") (gray-2 "#494949") (gray-3 "#5F5A60")
      (gray-4 "#A7A7A7") (gray-5 "#B0B3BA") (gray-6 "#B1B3BA") (gray-7 "#F8F8F8")

      (blue-1 "#0E2231") (blue-2 "#7587A6") (blue-3 "#8693A5") (blue-4 "#8996A8")
      (blue-5 "#8B98AB") (blue-6 "#AFC4DB") (blue-7 "#DDF0FF")

      (green-1 "#253B22") (green-2 "#8A9A95") (green-3 "#8F9D6A")
      (green-4 "#DAEFA3") (green-5 "#DDF2A4")

      (brown-1 "#4A410D") (brown-2 "#9B703F") (brown-3 "#AC885B")
      (brown-4 "#CDA869") (brown-5 "#E0C589")

      (orange-1 "#9B5C2E") (orange-2 "#CF7D34") (orange-3 "#CA7840") (orange-4 "#E9C062")

      (yellow-1 "#C5AF75") (yellow-2 "#DAD085") (yellow-3 "#F9EE98")

      (red-1 "#420E09") (red-2 "#CF6A4C") (red-3 "#D2A8A1")

      (purple-1 "#562D56") (purple-2 "#9B859D"))

  (custom-theme-set-faces
   'twilight
   `(default ((,class (:background ,gray-1 :foreground ,gray-7))))
   `(cursor ((,class (:background ,gray-4))))
   ;; Highlighting faces
   `(fringe ((,class (:background ,gray-1))))
   `(highlight ((,class (:background ,gray-1.2))))
   `(region ((,class (:background ,gray-1.2))))
   `(secondary-selection ((,class (:background ,blue-3))))
   `(isearch ((,class (:foreground ,gray-7 :background ,orange-3))))
   `(lazy-highlight ((,class (:background ,gray-3))))
   `(trailing-whitespace ((,class (:background ,red-3))))
   ;; Mode line faces
   `(mode-line ((,class
		 (:box (:line-width -1 :style released-button)
		  :background ,blue-7 :foreground ,gray-1))))
   `(mode-line-inactive ((,class
			  (:box (:line-width -1 :style released-button)
			   :background ,gray-2 :foreground ,gray-7))))
   `(compilation-mode-line-fail ((,class (:foreground ,red-2))))
   `(compilation-mode-line-run  ((,class (:foreground ,orange-3))))
   `(compilation-mode-line-exit ((,class (:foreground ,yellow-1))))
   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground ,yellow-2))))
   `(escape-glyph ((,class (:foreground ,gray-5))))
   `(error ((,class (:foreground ,red-2 :background ,red-3))))
   `(warning ((,class (:foreground ,orange-2))))
   `(success ((,class (:foreground ,green-3))))
   ;; Font lock faces
   `(font-lock-builtin-face ((,class (:foreground ,gray-6))))
   `(font-lock-comment-face ((,class (:foreground ,gray-3))))
   `(font-lock-constant-face ((,class (:foreground ,red-2))))
   `(font-lock-function-name-face ((,class (:foreground ,orange-1))))
   `(font-lock-keyword-face ((,class (:foreground ,brown-4))))
   `(font-lock-string-face ((,class (:foreground ,green-3))))
   `(font-lock-type-face ((,class (:foreground ,brown-2))))
   `(font-lock-variable-name-face ((,class (:foreground ,blue-2))))
   ;; Button and link faces
   `(link ((,class (:underline t :foreground ,blue-2))))
   `(link-visited ((,class (:underline t :foreground ,blue-3))))
   ;; Gnus faces
   `(gnus-group-news-1 ((,class (:foreground ,orange-4))))
   `(gnus-group-news-1-low ((,class (:foreground ,yellow-1))))
   `(gnus-group-news-2 ((,class (:foreground ,yellow-2))))
   `(gnus-group-news-2-low ((,class (:foreground ,orange-3))))
   `(gnus-group-news-3 ((,class (:foreground ,brown-5))))
   `(gnus-group-news-3-low ((,class (:foreground ,brown-3))))
   `(gnus-group-news-4 ((,class (:foreground ,yellow-3))))
   `(gnus-group-news-4-low ((,class (:foreground ,brown-3))))
   `(gnus-group-news-5 ((,class (:foreground ,orange-1))))
   `(gnus-group-news-5-low ((,class (:foreground ,orange-2))))
   `(gnus-group-news-low ((,class (:foreground ,gray-2))))
   `(gnus-group-mail-1 ((,class (:foreground ,orange-4))))
   `(gnus-group-mail-1-low ((,class (:foreground ,yellow-1))))
   `(gnus-group-mail-2 ((,class (:foreground ,yellow-2))))
   `(gnus-group-mail-2-low ((,class (:foreground ,orange-3))))
   `(gnus-group-mail-3 ((,class (:foreground ,brown-5))))
   `(gnus-group-mail-3-low ((,class (:foreground ,brown-3))))
   `(gnus-group-mail-low ((,class (:foreground ,gray-2))))
   `(gnus-header-content ((,class (:weight normal :foreground ,gray-4))))
   `(gnus-header-from ((,class (:foreground ,gray-2))))
   `(gnus-header-subject ((,class (:foreground ,brown-5))))
   `(gnus-header-name ((,class (:foreground ,yellow-2))))
   `(gnus-header-newsgroups ((,class (:foreground ,brown-3))))
   ;; Message faces
   `(message-header-name ((,class (:foreground ,yellow-2))))
   `(message-header-cc ((,class (:foreground ,gray-4))))
   `(message-header-other ((,class (:foreground ,brown-3))))
   `(message-header-subject ((,class (:foreground ,brown-5))))
   `(message-header-to ((,class (:foreground ,gray-2))))
   `(message-cited-text ((,class (:foreground ,brown-5))))
   `(message-separator ((,class (:foreground ,orange-4))))
   ;; SMerge faces
   `(smerge-refined-change ((,class (:background ,blue-1))))
   ;; Ediff faces
   `(ediff-current-diff-A ((,class (:background ,gray-2))))
   `(ediff-fine-diff-A ((,class (:background ,purple-1))))
   `(ediff-even-diff-A ((,class (:background ,gray-3))))
   `(ediff-odd-diff-A ((,class (:background ,gray-3))))
   `(ediff-current-diff-B ((,class (:background ,gray-2))))
   `(ediff-fine-diff-B ((,class (:background ,purple-2))))
   `(ediff-even-diff-B ((,class (:background ,gray-3))))
   `(ediff-odd-diff-B ((,class (:background ,gray-3))))
   ;; Flyspell faces
   `(flyspell-duplicate ((,class (:underline ,orange-2))))
   `(flyspell-incorrect ((,class (:underline ,red-3))))
   ;; Semantic faces
   `(semantic-decoration-on-includes ((,class (:underline ,blue-4))))
   `(semantic-decoration-on-private-members-face
     ((,class (:background ,brown-1))))
   `(semantic-decoration-on-protected-members-face
     ((,class (:background ,purple-1))))
   `(semantic-decoration-on-unknown-includes
     ((,class (:background ,red-3))))
   `(semantic-decoration-on-unparsed-includes
     ((,class (:background ,gray-3))))
   `(semantic-tag-boundary-face ((,class (:overline ,yellow-1))))
   `(semantic-unmatched-syntax-face ((,class (:underline ,red-3)))))

  (custom-theme-set-variables
   'twilight
   `(ansi-color-names-vector [,blue-1 ,red-2 ,green-3 ,gray-4
			      ,blue-2 ,purple-2 ,orange-4 ,gray-7])))

(provide-theme 'twilight)
