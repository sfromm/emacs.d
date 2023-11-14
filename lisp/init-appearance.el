;;; init-appearance.el --- Init appearance pieces -*- lexical-binding: t -*-
;; Copyright (C) 2021, 2022 by Stephen Fromm

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


(defcustom forge-font "IBM Plex Mono"
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

(defun my-font-name-and-size ()
  "Compute and return font name and size string."
  (interactive)
  (let* ((size (number-to-string forge-font-size))
         (name (concat forge-font "-" size)))
    (if (interactive-p) (message "Font: %s" name))
    name))

(defun font-ok-p ()
  "Is configured font valid?"
  (interactive)
  (member forge-font (font-family-list)))

(defun font-size-increase ()
  "Increase font size."
  (interactive)
  (setq forge-font-size (+ forge-font-size 1))
  (my-font-update))

(defun font-size-decrease ()
  "Decrease font size."
  (interactive)
  (setq forge-font-size (- forge-font-size 1))
  (my-font-update))

(defun my-font-update ()
  "Update font configuration."
  (interactive)
  (when (font-ok-p)
    (progn
      (message "Font: %s" (my-font-name-and-size))
      ;; (set-frame-font forge-font)
      (set-face-attribute 'default nil :family forge-font :weight 'semi-light :height (* forge-font-size 10))
      (set-face-attribute 'fixed-pitch nil :family forge-font :height 1.0)
      (when forge-variable-pitch-font
        (set-face-attribute 'variable-pitch nil :family forge-variable-pitch-font :height forge-variable-pitch-scale))
      (when (fboundp 'set-fontset-font) ;; from doom-emacs
        (dolist (font (append (list forge-unicode-font) forge-unicode-extra-fonts))
          (set-fontset-font t 'unicode (font-spec :family font) nil 'prepend))))))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(defun forge/emoji-shrug () "Shrug emoji." (interactive) (insert "¯\\_(ツ)_/¯"))
(defun forge/emoji-glare () "Glare emoji." (interactive) (insert "ಠ_ಠ"))
(defun forge/emoji-table-flip () "Table fip emoji." (interactive) (insert "(╯°□°）╯︵ ┻━┻"))

(use-package emojify
  :init (setq emojify-emojis-dir (expand-file-name "emojis" forge-state-dir)))

(use-package rainbow-mode)


(defcustom forge-theme 'modus-operandi
  "Preferred graphics theme."
  :type 'symbol
  :group 'forge)

;; https://github.com/hlissner/emacs-solaire-mode
;; Encouraged by doom-themes
(use-package solaire-mode
  :disabled t)

;; https://github.com/hlissner/emacs-doom-themes
(use-package doom-themes
  :config
  (doom-themes-org-config))

;; https://github.com/dracula/emacs
(use-package dracula-theme)

;; https://github.com/fniessen/emacs-leuven-theme
(use-package leuven-theme)

;; https://github.com/cpaulik/emacs-material-theme
(use-package material-theme)

;; https://gitlab.com/protesilaos/ef-themes
(use-package ef-themes
  :custom
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t))

;; https://gitlab.com/protesilaos/modus-themes
(use-package modus-themes
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t))

;; https://github.com/rougier/nano-theme
(use-package nano-theme)

;; https://github.com/kunalb/poet
(use-package poet-theme)

;; https://github.com/bbatsov/solarized-emacs
(use-package solarized-theme
  :custom
  (solarized-use-variable-pitch t)
  (solarized-scale-org-headlines t))

;; https://github.com/ianpan870102/tron-legacy-emacs-theme
(use-package tron-legacy-theme
  :custom
  (tron-legacy-theme-vivid-cursor t)
  (tron-legacy-theme-softer-bg t))

;; https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme
  :custom
  (zenburn-use-variable-pitch t)
  (zenburn-scale-org-headlines t))


;; https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :custom
  (doom-modeline-github nil "Disable github integration")
  (doom-modeline-buffer-file-name-style 'buffer-name)
  (doom-modeline-lsp nil "Disable integration with lsp")
  (doom-modeline-workspace-name t)
  :hook
  (doom-modeline-mode . column-number-mode)
  (doom-modeline-mode . size-indication-mode)
  (after-init . doom-modeline-mode))

(use-package nyan-mode)


(defun forge/setup-ui ()
  "Set up the look and feel."
  (interactive)
  (when forge-theme
    (load-theme forge-theme t))
  (setq visible-bell t                 ;; set a visible bell ...
        ring-bell-function #'ignore)    ;; and squash the audio bell
  (when (display-graphic-p)
    (when (forge/system-type-darwin-p)
      (setq frame-resize-pixelwise t))  ;; allow frame resizing by pixels, instead of character dimensions
    (my-font-update)
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

(when (daemonp)
  (add-hook 'after-make-frame-functions #'forge/setup-ui-in-daemon))
(add-hook 'after-init-hook #'forge/setup-ui)

(use-package lin
  :config
  (setq lin-face 'lin-mac-override-fg)
  (dolist (hook '(dired-mode-hook
                  elfeed-search-mode-hook
                  log-view-mode-hook
                  magit-log-mode-hook
                  notmuch-search-mode-hook
                  notmuch-tree-mode-hook
                  occur-mode-hook
                  org-agenda-mode-hook
                  package-menu-mode-hook))
    (add-hook hook #'lin-mode)))

(provide 'init-appearance)
