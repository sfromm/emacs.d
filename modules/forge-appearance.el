;;; forge-appearance.el --- Set up appearance.  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Stephen Fromm

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
(defvar forge-font-name "Fira Code"
  "Preferred font.")

(defvar forge-font-size 12
  "Preferred font size.")

(defun forge/font-ok-p ()
  "Is configured font valid?"
  (interactive)
  (member forge-font-name (font-family-list)))

(defun forge/font-name-and-size ()
  "Compute font name and size string."
  (interactive)
  (let* ((size (number-to-string forge-font-size))
          (name (concat forge-font-name "-" size)))
    name))

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

(defun forge/set-emoji-font ()
  "Try to set emoji font properly."
  (interactive)
  (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend))

(defun forge/font-update ()
  "Update font configuration."
  (interactive)
  (if (forge/font-ok-p)
    (progn
      (message "Setting font to: %s" (forge/font-name-and-size))
      (set-frame-font forge-font-name)
      (set-face-attribute 'default nil :font forge-font-name :height (* forge-font-size 10))
      (set-face-font 'default forge-font-name)
      (forge/set-emoji-font))))

(forge/font-update)

(use-package all-the-icons :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))


;;;
;;; Themes
;;;
(defvar forge-theme nil
  "Preferred graphics theme.")

;; Install a mix of themes to have something to start with.
(dolist (p '(doom-themes leuven-theme
              material-theme solarized-theme
              spacemacs-theme zenburn-theme))
  (progn (forge/package-install p)))

;;;
;;; Modeline
;;;
(use-package smart-mode-line
  :ensure t
  :disabled t
  :defer t
  :config
  (add-hook 'after-load-theme-hook 'smart-mode-line-enable)
  (setq
    sml/no-confirm-load-theme t
    sml/theme 'dark
    sml/mode-width 'full
    sml/name-width 30
    sml/shorten-modes t)
  (sml/setup))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-init))

(use-package rainbow-mode
  :ensure t
  :defer t)

(defun forge/setup-ui ()
  "Set up the look and feel."
  (interactive)
  (when forge-theme
    (load-theme forge-theme t))
  (when (display-graphic-p)
    (forge/font-update)
    (line-number-mode t)                ;; show line number in modeline
    (column-number-mode t)              ;; show column number in modeline
    (size-indication-mode t)            ;; show buffer size in modeline
    (tool-bar-mode -1)                  ;; disable toolbar
    (scroll-bar-mode -1)                ;; disable scroll bar
    (display-battery-mode)))

(defun forge/setup-up-in-daemon (frame)
  "Reload the UI in a daemon frame FRAME."
  (when (or (daemonp) (not (display-graphic-p)))
    (with-selected-frame frame
      (run-with-timer 0.1 nil #'forge/setup-ui))))

(add-hook 'after-make-frame-functions 'forge/setup-ui-in-daemon)
(forge/setup-ui)

(provide 'forge-appearance)
;;; forge-appearance ends here