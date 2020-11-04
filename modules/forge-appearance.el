;;; forge-appearance.el --- Set up appearance.  -*- lexical-binding: t -*-

;; Copyright (C) 2018-2020 Stephen Fromm

;; Author: Stephen Fromm

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;;
;;; Fonts
;;;
(defcustom forge-font "Fira Code"
  "Preferred default font."
  :type 'string
  :group 'forge)

(defcustom forge-font-size 12
  "Preferred font size."
  :type 'integer
  :group 'forge)

(defcustom forge-variable-pitch-font "Fira Sans"
  "Preferred variable pitch font."
  :type 'string
  :group 'forge)

(defcustom forge-variable-pitch-scale 1.1
  "Preferred variable pitch font."
  :type 'decimal
  :group 'forge)

(defcustom forge-unicode-font "Fira Sans"
  "Preferred Unicode font.  This takes precedence over `forge-unicode-extra-fonts'."
  :type 'string
  :group 'forge)

(defvar forge-unicode-extra-fonts
  (list "all-the-icons"
        "FontAwesome"
        "github-octicons"
        "Weather Icons")
  "List of extra Unicode fonts.")

(defun forge/font-name-and-size ()
  "Compute font name and size string."
  (interactive)
  (let* ((size (number-to-string forge-font-size))
         (name (concat forge-font "-" size))) name))

(defun forge/font-ok-p ()
  "Is configured font valid?"
  (interactive)
  (member forge-font (font-family-list)))

(defun forge/font-size-increase ()
  "Increase font size."
  (interactive)
  (setq forge-font-size (+ forge-font-size 1))
  (forge/font-update))

(defun forge/font-size-decrease ()
  "Decrease font size."
  (interactive)
  (setq forge-font-size (- forge-font-size 1))
  (forge/font-update))

(defun forge/font-update ()
  "Update font configuration."
  (interactive)
  (when (forge/font-ok-p)
    (progn
      (message "Font: %s" (forge/font-name-and-size))
      ;; (set-frame-font forge-font)
      (set-face-attribute 'default nil :family forge-font :height (* forge-font-size 10))
      (set-face-attribute 'fixed-pitch nil :family forge-font :height 1.0)
      (when forge-variable-pitch-font
        (set-face-attribute 'variable-pitch nil :family forge-variable-pitch-font :height forge-variable-pitch-scale))
      (when (fboundp 'set-fontset-font) ;; from doom-emacs
        (dolist (font (append (list forge-unicode-font) forge-unicode-extra-fonts))
          (set-fontset-font t 'unicode (font-spec :family font) nil 'prepend))))))

(forge/font-update)

;;
;; all the icons
;; https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :hook
  (dired-mode . all-the-icons-dired-mode))


;;;
;;; Themes
;;;
(defun forge/install-themes ()
  "Install a mix of themes."
  (interactive)
  (dolist (p '(doom-themes           ;; https://github.com/hlissner/emacs-doom-themes
               leuven-theme          ;; https://github.com/fniessen/emacs-leuven-theme
               material-theme        ;; https://github.com/cpaulik/emacs-material-theme
               modus-operandi-theme  ;; https://gitlab.com/protesilaos/modus-themes
               modus-vivendi-theme   ;; https://gitlab.com/protesilaos/modus-themes
               poet-theme            ;; https://github.com/kunalb/poet
               solarized-theme       ;; https://github.com/bbatsov/solarized-emacs
               spacemacs-theme       ;; https://github.com/nashamri/spacemacs-theme
               zenburn-theme))       ;; https://github.com/bbatsov/zenburn-emacs
    (progn (forge/package-install p)))
  (when (forge/system-type-darwin-p)
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))))

(forge/install-themes)

(defcustom forge-theme 'modus-operandi
  "Preferred graphics theme."
  :type 'symbol
  :group 'forge)

(use-package zenburn-theme
  :defer t
  :disabled t
  :custom
  (zenburn-use-variable-pitch t)
  (zenburn-scale-org-headlines t)
  :config
  (zenburn-with-color-variables
   (custom-set-faces
    ;; elfeed
    `(elfeed-search-date-face ((t (:foreground ,zenburn-orange))))
    `(elfeed-search-feed-face ((t (:foreground ,zenburn-yellow))))
    `(elfeed-search-tag-face ((t (:foreground ,zenburn-fg+2))))
    `(elfeed-search-title-face ((t (:foreground ,zenburn-fg))))
    `(elfeed-search-unread-count-face ((t (:foreground ,zenburn-cyan :bold nil))))
    `(elfeed-search-unread-title-face ((t (:foreground ,zenburn-fg+2 :bold nil))))
    ;; notmuch
    '(notmuch-hello-logo-background ((t (:inherit 'default))))
    `(notmuch-message-summary-face ((t (:foreground ,zenburn-fg :background ,zenburn-bg-2 :weight bold))))
    `(notmuch-search-count ((t (:inherit 'default))))
    `(notmuch-search-date ((t (:inherit 'default))))
    `(notmuch-search-matching-authors ((t (:inherit 'default))))
    `(notmuch-search-non-matching-authors ((t (:inherit 'default))))
    `(notmuch-search-subject ((t (:inherit 'default))))
    `(notmuch-search-unread-face ((t (:weight bold))))
    `(notmuch-search-flagged-face ((t (:foreground ,zenburn-blue))))
    `(notmuch-tag-added ((t (:underline ,zenburn-green))))
    `(notmuch-tag-deleted ((t (:strike-through ,zenburn-red))))
    `(notmuch-tag-face ((t (:foreground ,zenburn-yellow))))
    `(notmuch-tag-flagged ((t (:foreground ,zenburn-blue))))
    `(notmuch-tag-unread ((t (:foreground ,zenburn-red))))
    `(notmuch-tree-match-author-face ((t (:foreground ,zenburn-blue))))
    `(notmuch-tree-match-date-face ((t (:foreground ,zenburn-yellow))))
    `(notmuch-tree-match-tag-face ((t (:foreground ,zenburn-cyan))))
    `(notmuch-tree-no-match-face ((t (:inherit font-lock-comment-face))))
    )))

(use-package solarized-theme
  :defer t
  :custom
  (solarized-use-variable-pitch t)
  (solarized-scale-org-headlines t))

(use-package doom-themes
  :defer t
  :config
  (doom-themes-org-config))

(use-package modus-operandi-theme
  :custom
  (modus-operandi-theme-scale-headings t))

(use-package modus-vivendi-theme
  :custom
  (modus-vivendi-theme-scale-headings t))


;;;
;;; Modeline
;;;

;; https://github.com/milkypostman/powerline
(use-package powerline
  :disabled t
  :ensure t
  :custom
  (powerline-default-separator 'slant)
  (powerline-default-separator-dir (quote (left . right)))
  (powerline-display-buffer-size nil)
  (powerline-display-hud nil)
  (powerline-display-mule-info nil)
  (powerline-gui-use-vcs-glyph t)
  :hook
  (after-init . powerline-default-theme))

;; https://github.com/Malabarba/smart-mode-line
(use-package smart-mode-line
  :disabled t
  :ensure t
  :custom
  (sml/no-confirm-load-theme t)
  (sml/theme 'respectful)
  (sml/mode-width 'full)
  (sml/name-width 30)
  (sml/shorten-modes t)
  :hook
  (after-load-theme . smart-mode-line-enable)
  (after-init . sml/setup))

;; https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-github nil "Disable github integration")
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-lsp nil "Disable integration with lsp")
  :hook
  (doom-modeline-mode . column-number-mode)
  (doom-modeline-mode . size-indication-mode)
  (after-init . doom-modeline-mode))

;;
(use-package nyan-mode
  :ensure t
  :disabled t
  :defer t
  :hook (doom-modeline-mode . nyan-mode))


;;; misc
;;;
;;;
(use-package rainbow-mode
  :ensure t
  :defer t)


;;;
;;;
;;;
(defun forge/setup-ui ()
  "Set up the look and feel."
  (interactive)
  (when forge-theme
    (load-theme forge-theme t))
  (when (display-graphic-p)
    (when (forge/system-type-darwin-p)
      (setq frame-resize-pixelwise t))  ;; allow frame resizing by pixels, instead of character dimensions
    (forge/install-themes)
    (forge/font-update)
    (line-number-mode t)                ;; show line number in modeline
    (column-number-mode t)              ;; show column number in modeline
    (size-indication-mode t)            ;; show buffer size in modeline
    (tool-bar-mode -1)                  ;; disable toolbar
    (scroll-bar-mode -1)                ;; disable scroll bar
    (display-battery-mode)))

(defun forge/setup-ui-in-daemon (frame)
  "Reload the UI in a daemon frame FRAME."
  (when (or (daemonp) (not (display-graphic-p)))
    (with-selected-frame frame
      (run-with-timer 0.1 nil #'forge/setup-ui))))

(defun forge/transparency (value)
  "Set the transparency of the frame window with VALUE 0=transparent/100=opaque."
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(add-hook 'after-make-frame-functions 'forge/setup-ui-in-daemon)
(forge/setup-ui)

(provide 'forge-appearance)
;;; forge-appearance ends here
