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
(defcustom forge-font "Hack"
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

(defcustom forge-unicode-font "Fira Sans"
  "Preferred Unicode font."
  :type 'string
  :group 'forge)

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
      (set-face-attribute 'fixed-pitch nil :family forge-font :height (* forge-font-size 10))
      (when forge-variable-pitch-font
        (set-face-attribute 'variable-pitch nil :family forge-variable-pitch-font))
      (when (fontp forge-unicode-font)
        (set-fontset-font t 'unicode (font-spec :family forge-unicode-font) nil 'prepend)))))

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

(defcustom forge-theme nil
  "Preferred graphics theme."
  :type 'symbol
  :group 'forge)

(unless forge-theme
  (setq forge-theme 'doom-darktooth))

(use-package zenburn-theme
  :defer t
  :custom
  (zenburn-use-variable-pitch t)
  (zenburn-scale-org-headlines t))

(use-package solarized-theme
  :defer t
  :custom
  (solarized-use-variable-pitch t)
  (solarized-scale-org-headlines t))

(use-package doom-themes
  :defer t
  :config
  (doom-themes-org-config))



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
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-lsp nil "Disable integration with lsp")
  :hook
  (doom-modeline-mode . column-number-mode)
  (doom-modeline-mode . size-indication-mode)
  (after-init . doom-modeline-mode))

;; https://github.com/dbordak/telephone-line
(use-package telephone-line
  :ensure t
  :disabled t
  :custom
  (telephone-line-primary-left-separator 'telephone-line-gradient)
  (telephone-line-secondary-left-separator 'telephone-line-nil)
  (telephone-line-primary-right-separator 'telephone-line-gradient)
  (telephone-line-secondary-right-separator 'telephone-line-nil)
  (telephone-line-height 24)
  :config
  (telephone-line-defsegment forge/telephone-line-space ()
    "A space."
    (telephone-line-raw "   "))
  (telephone-line-defsegment forge/telephone-line-kbd-macro-segment ()
    "Offer keyboard macro feedback."
    (when (or defining-kbd-macro executing-kbd-macro)
      (telephone-line-raw "Macro")))
  (setq telephone-line-lhs '((accent . (forge/telephone-line-kbd-macro-segment
                                        telephone-line-vc-segment
                                        telephone-line-erc-modified-channels-segment
                                        telephone-line-process-segment))
                             (nil . (telephone-line-projectile-segment telephone-line-buffer-segment)))
        telephone-line-rhs '((nil . (telephone-line-flycheck-segment telephone-line-misc-info-segment))
                             (evil . (telephone-line-major-mode-segment))
                             (accent . (telephone-line-airline-position-segment))))
  (telephone-line-mode 1))

;;
(use-package nyan-mode
  :ensure t
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
