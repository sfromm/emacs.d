;;; doom-filbert-light-theme.el --- attempt at a light version of darktooth -*- no-byte-compile: t; -*-
(require 'doom-themes)

(defgroup doom-filbert-light-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-filbert-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-filbert-light-theme
  :type 'boolean)

(defcustom doom-filbert-light-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-filbert-light-theme
  :type 'boolean)

(defcustom doom-filbert-light-comment-bg doom-filbert-light-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-filbert-light-theme
  :type 'boolean)

(defcustom doom-filbert-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-filbert-light-theme
  :type '(choice integer boolean))

;;
(def-doom-theme doom-filbert-light
  "A light theme inspired by Darktooth"

  ;; name        default   256       16
  ((bg         '("#fefcef" "#ffffaf" "brightwhite"  ))
   (bg-alt     '("#fef9e0" "#ffdfaf" "white"        ))
   (base0      '("#d5c4a1" "#bcbcbc" "white"        ))
   (base1      '("#bdae93" "#a8a8a8" "white"  ))
   (base2      '("#a89984" "#949494" "brightblack"  ))
   (base3      '("#928374" "#8a8a8a" "brightblack"  ))
   (base4      '("#7c6f64" "#767676" "brightblack"  ))
   (base5      '("#665c54" "#626262" "brightblack"  ))
   (base6      '("#504945" "#4e4e4e" "brightblack"  ))
   (base7      '("#3c3836" "#3a3a3a" "brightblack"  ))
   (base8      '("#32302f" "#303030" "black"        ))
   (fg         '("#282828" "#262626" nil            ))
   (fg-alt     '("#1d2021" "#1c1c1c" nil            ))

   (grey       base5)
   (red        '("#FB4934" "#D75F5F" "red"          ))
   (orange     '("#FE8019" "#FF8700" "brightred"    ))
   (yellow     '("#FABD2F" "#FFAF00" "yellow"       ))
   (green      '("#b8bb26" "#73af00" "green"        ))
   (teal       '("#8ec07c" "#87af87" "brightgreen"  ))
   (blue       '("#83A598" "#87AFAF" "brightblue"   ))
   (magenta    '("#D3879B" "#d787af" "brightmagenta"))
   (violet     '("#D3869B" "#D787AF" "magenta"      ))
   (cyan       '("#3FD7E5" "#00d7ff" "brightcyan"   ))

   (bright-red      '("#FB4933" "#d75f5f" nil ))
   (bright-orange   '("#FE8019" "#ff8700" nil ))
   (bright-yellow   '("#FABD2F" "#ffaf00" nil ))
   (bright-green    '("#B8BB26" "#afaf00" nil ))
   (bright-teal     '("#8EC07C" "#87af87" nil ))
   (bright-blue     '("#83A598" "#87afaf" nil ))
   (bright-purple   '("#D3869B" "#d787af" nil ))
   (bright-cyan     '("#3FD7E5" "#00d7ff" nil ))

   (neutral-red     '("#FB4934" "#D75F5F" nil ))
   (neutral-orange  '("#FE8019" "#FF8700" nil ))
   (neutral-yellow  '("#FABD2F" "#FFAF00" nil ))
   (neutral-green   '("#B8BB26" "#73AF00" nil ))
   (neutral-teal    '("#8EC07C" "#87AF87" nil ))
   (neutral-blue    '("#83A598" "#87AFAF" nil ))
   (neutral-purple  '("#D3869B" "#D787AF" nil ))
   (neutral-cyan    '("#17CCD5" "#17CCD5" nil ))

   (faded-red       '("#9D0006" "#870000" nil ))
   (faded-orange    '("#AF3A03" "#af5f00" sienna ))
   (faded-yellow    '("#B57614" "#af8700" nil    ))
   (faded-green     '("#79740E" "#878700" nil    ))
   (faded-teal      '("#427B58" "#5f8787" nil    ))
   (faded-blue      '("#076678" "#005f87" nil    ))
   (faded-purple    '("#8F3F71" "#875f87" nil    ))
   (faded-cyan      '("#00A7AF" "#00afaf" nil    ))

   (muted-red       '("#901A1E" "#870000" nil    ))
   (muted-orange    '("#A24921" "#af5f00" nil    ))
   (muted-yellow    '("#A87933" "#af8700" nil    ))
   (muted-green     '("#556C21" "#878700" nil    ))
   (muted-teal      '("#506E59" "#5f8787" nil    ))
   (muted-blue      '("#1B5C6B" "#005f87" nil    ))
   (muted-purple    '("#82526E" "#875f87" nil    ))
   (muted-cyan      '("#18A7AF" "#00afaf" nil    ))

   (dark-red        '("#421E1E" "#5f0000" nil    ))
   (dark-orange     '("#613620" "#af5f00" nil    ))
   (dark-yellow     '("#4D3B27" "#5f5f00" nil    ))
   (dark-green      '("#232B0F" "#005f00" nil    ))
   (dark-teal       '("#36473A" "#005f5f" nil    ))
   (dark-blue       '("#2B3C44" "#00005f" nil    ))
   (dark-purple     '("#4E3D45" "#5f00af" nil    ))
   (dark-cyan       '("#205161" "#005f87" nil    ))

   ;; face categories -- required for all themes
   (highlight      cyan)
   (vertical-bar   (doom-darken bg 0.25))
   (selection      neutral-blue)
   (builtin        orange)
   (comments       (if doom-filbert-light-brighter-comments muted-cyan base5))
   (doc-comments   (doom-lighten (if doom-filbert-light-brighter-comments muted-cyan base5) 0.25))
   (constants      base5)
   (functions      blue)
   (keywords       orange)
   (methods        blue)
   (operators      fg)
   (type           yellow)
   (strings        green)
   (variables      blue)
   (numbers        orange)
   (region         selection)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg-alt) "black" "black"))
   (-modeline-bright doom-filbert-light-brighter-modeline)
   (-modeline-pad
    (when doom-filbert-light-padded-modeline
      (if (integerp doom-filbert-light-padded-modeline) doom-filbert-light-padded-modeline 4)))


   ;; --- Modeline config -------------------
   (modeline-fg     nil)
   (modeline-fg-alt (doom-blend cyan base4 (if -modeline-bright 0.5 0.2)))

   (modeline-bg
    (if -modeline-bright
        (doom-darken base3 0.05)
      base1))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken base3 0.1)
      base1))
   (modeline-bg-inactive (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(doom-darken (car bg-alt) 0.05) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background bg  :foreground "#1e2022")

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-filbert-light-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground fg)
   (css-selector             :foreground red)

   ;; elfeed
   (elfeed-search-date-face  :foreground blue)
   (elfeed-search-feed-face  :foreground cyan)
   (elfeed-search-title-face :foreground base5)

   ;; eshell
   (eshell-prompt            :foreground orange :weight 'bold)

   ;; magit
   (magit-diff-hunk-heading-highlight :background orange)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-darken bg 0.1))

   ;; notmuch
   (notmuch-hello-logo-background :background bg-alt)
   (notmuch-message-summary-face :foreground fg-alt :background bg-alt)
   (notmuch-tag-deleted :strike-through red :foreground red)

   ;; org-mode
   (org-block            :background (doom-darken bg-alt 0.04))
   (org-block-begin-line :foreground base4 :slant 'italic :background (doom-darken bg 0.04))

   ((outline-1 &override) :foreground orange :weight 'ultra-bold)
   ((outline-2 &override) :foreground (doom-blend fg orange 0.35))
   ((outline-3 &override) :foreground (doom-blend fg orange 0.7))
   ((outline-4 &override) :foreground blue)
   ((outline-5 &override) :foreground (doom-blend magenta blue 0.2))
   ((outline-6 &override) :foreground (doom-blend magenta blue 0.4))
   ((outline-7 &override) :foreground (doom-blend magenta blue 0.6))
   ((outline-8 &override) :foreground fg)

   ;;   (org-ellipsis          :underline nil :background base4    :foreground red)
   ((org-quote &override) :background base1)

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden))

  ;;   (tooltip              :background bg-alt :foreground fg))

  ;; --- extra variables ---------------------
  ()
  )

;; Local Variables:
;; eval: (when (require 'rainbow-mode nil t) (rainbow-mode 1))
;; End:
;;; doom-filbert-light-theme.el ends here
