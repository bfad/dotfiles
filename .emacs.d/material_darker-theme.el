;;; material_darker-theme.el --- an Emacs 24 theme based on Material Theme (tmTheme)
;;
;;; Author: Auto Converted to Emacs 24 by tmtheme-to-deftheme (tm2deftheme)
;;; Version: 1463347018
;;; Original author: Mattia Astorino (http://astorinomattia.it)
;;; Url: https://github.com/emacsfodder/tmtheme-to-deftheme
;;; Package-Requires: ((emacs "24.0"))
;;
;;; Commentary:
;;  This theme was automatically generated by tmtheme-to-deftheme (tm2deftheme),
;;  from Material Theme (tmTheme) by Mattia Astorino (http://astorinomattia.it)
;;
;;; Code:

(deftheme material_darker
  "material_darker-theme - Created by tmtheme-to-deftheme - 2016-05-15 17:16:58 -0400")

(custom-theme-set-variables
 'material_darker
)

(custom-theme-set-faces
 'material_darker
 ;; basic theming.

 '(default ((t (:foreground "#eeffffff" :background "#212121" ))))
 '(region  ((t (:background "#505050"))))
 '(cursor  ((t (:background "#FFCC00"))))

 ;; Temporary defaults
 '(linum                               ((t (:foreground "#656565"  :background "#353737" ))))
 '(fringe                              ((t (                       :background "#353737" ))))

 '(minibuffer-prompt                   ((t (:foreground "#1278A8"  :background nil       :weight bold                                  ))))
 '(escape-glyph                        ((t (:foreground "orange"   :background nil                                                     ))))
 '(highlight                           ((t (:foreground "orange"   :background nil                                                     ))))
 '(shadow                              ((t (:foreground "#777777"  :background nil                                                     ))))

 '(trailing-whitespace                 ((t (:foreground "#FFFFFF"  :background "#C74000"                                               ))))
 '(link                                ((t (:foreground "#00b7f0"  :background nil       :underline t                                  ))))
 '(link-visited                        ((t (:foreground "#4488cc"                        :underline t :inherit (link)                  ))))
 '(button                              ((t (:foreground "#FFFFFF"  :background "#444444" :underline t :inherit (link)                  ))))
 '(next-error                          ((t (                                             :inherit (region)                             ))))
 '(query-replace                       ((t (                                             :inherit (isearch)                            ))))
 '(header-line                         ((t (:foreground "#EEEEEE"  :background "#444444" :box nil :inherit (mode-line)                 ))))

 '(mode-line-highlight                 ((t (                                             :box nil                                      ))))
 '(mode-line-emphasis                  ((t (                                             :weight bold                                  ))))
 '(mode-line-buffer-id                 ((t (                                             :box nil :weight bold                         ))))

 '(mode-line-inactive                  ((t (:foreground "#4a4d4d"  :background "#353737" :box nil :weight light :inherit (mode-line)   ))))
 '(mode-line                           ((t (:foreground "#3388cc"  :background "#353737" :box nil ))))

 '(isearch                             ((t (:foreground "#99ccee"  :background "#444444"                                               ))))
 '(isearch-fail                        ((t (                       :background "#ffaaaa"                                               ))))
 '(lazy-highlight                      ((t (                       :background "#77bbdd"                                               ))))
 '(match                               ((t (                       :background "#3388cc"                                               ))))

 '(tooltip                             ((t (:foreground "black"    :background "LightYellow" :inherit (variable-pitch)                 ))))

 '(js3-function-param-face             ((t (:foreground "#BFC3A9"                                                                      ))))
 '(js3-external-variable-face          ((t (:foreground "#F0B090"  :bold t                                                             ))))

 '(secondary-selection                 ((t (                       :background "#342858"                                               ))))
 '(cua-rectangle                       ((t (:foreground "#E0E4CC"  :background "#342858" ))))

 ;; Magit hightlight
 '(magit-item-highlight                ((t (:foreground "white" :background "#1278A8" :inherit nil ))))

 ;; flyspell-mode
 '(flyspell-incorrect                  ((t (:underline "#AA0000" :background nil :inherit nil ))))
 '(flyspell-duplicate                  ((t (:underline "#009945" :background nil :inherit nil ))))

 ;; flymake-mode
 '(flymake-errline                     ((t (:underline "#AA0000" :background nil :inherit nil ))))
 '(flymake-warnline                    ((t (:underline "#009945" :background nil :inherit nil ))))

 ;;git-gutter
 '(git-gutter:added                    ((t (:foreground "#609f60" :bold t))))
 '(git-gutter:modified                 ((t (:foreground "#3388cc" :bold t))))
 '(git-gutter:deleted                  ((t (:foreground "#cc3333" :bold t))))

 '(diff-added                          ((t (:background "#305030"))))
 '(diff-removed                        ((t (:background "#903010"))))
 '(diff-file-header                    ((t (:background "#362145"))))
 '(diff-context                        ((t (:foreground "#E0E4CC"))))
 '(diff-changed                        ((t (:foreground "#3388cc"))))
 '(diff-hunk-header                    ((t (:background "#242130"))))


 '(font-lock-comment-face ((t (:foreground "#656565"  :italic t))))
 '(font-lock-keyword-face ((t (:foreground "#c792ea"  ))))
 '(font-lock-variable-name-face ((t (:foreground "#B2CCD6"  ))))
 '(font-lock-function-name-face ((t (:foreground "#82AAFF"  ))))
 '(font-lock-type-face ((t (:foreground "#89DDFF"  ))))
 '(font-lock-string-face ((t (:foreground "#C3E88D"  ))))
 '(font-lock-constant-face ((t (:foreground "#F77669"  ))))
 '(diff-added ((t (:foreground "#F1E655"  ))))
 '(diff-removed ((t (:foreground "#ff5370"  ))))
 '(diff-changed ((t (:foreground "#c792ea"  ))))
 '(error ((t (:foreground "#ffffff" :background "#EC5F67" ))))
 '(font-lock-warning-face ((t (:foreground "#ffffff" :background "#d3423e" ))))
 '(git-gutter:deleted ((t (:foreground "#EC5F67"  ))))
 '(git-gutter:modified ((t (:foreground "#FFCF1B"  ))))
 '(git-gutter:added ((t (:foreground "#C3E88D"  ))))
 '(git-gutter:untracked ((t (:foreground "#546E7A"  ))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#656565"  :italic t))))

;; Rainbow delimiters
 ;'(rainbow-delimiters-depth-1-face ((t (:foreground "#b33b4f"))))
 ;'(rainbow-delimiters-depth-2-face ((t (:foreground "#c14459"))))
 ;'(rainbow-delimiters-depth-3-face ((t (:foreground "#c75669"))))
 ;'(rainbow-delimiters-depth-4-face ((t (:foreground "#cd6879"))))
 ;'(rainbow-delimiters-depth-5-face ((t (:foreground "#d37a89"))))
 ;'(rainbow-delimiters-depth-6-face ((t (:foreground "#d98c99"))))
 ;'(rainbow-delimiters-depth-7-face ((t (:foreground "#df9ea8"))))
 ;'(rainbow-delimiters-depth-8-face ((t (:foreground "#e5afb8"))))
 ;'(rainbow-delimiters-depth-9-face ((t (:foreground "#ebc1c8"))))
 ;'(rainbow-delimiters-unmatched-face ((t (:foreground "#FF0000"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#e91e63"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#2196F3"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#EF6C00"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#B388FF"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#76ff03"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#26A69A"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#FFCDD2"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#795548"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#DCE775"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "#ffffff" :background "#EC5F67" ))))

 '(company-preview ((t (:foreground "#546E7A" :background "#362145"))))
 '(company-preview-common ((t (:foreground "#546E7A" :background "#362145"))))
 '(company-scrollbar-bg ((t (:background "#656565"))))
 '(company-scrollbar-fg ((t (:background "#B388FF"))))
 '(company-template-field ((t (:background "#362145"))))
 '(company-tooltip ((t (:weight bold :foreground "#3388CC" :background "#362145"))))
 '(company-tooltip-selection ((t (:background "#4A4D4D"))))
 '(company-tooltip-annotation ((t (:weight normal :foreground "#546E7A"))))
 '(company-tooltip-common ((t (:weight normal :inherit company-tooltip))))
 '(company-tooltip-common-selection ((t (:weight normal :inherit company-tooltip-selection))))
 ) ;; End face definitions

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'material_darker)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode +1))
;; End:

;;; material_darker-theme_darker ends here
