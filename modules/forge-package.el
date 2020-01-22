;;; forge-package.el --- Set up packages.  -*- lexical-binding: t -*-

;; Copyright (C) 2018, 2019 Stephen Fromm

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
(defvar forge-core-packages '(use-package quelpa quelpa-use-package)
  "A list of core packages that will be automatically installed.")

(defvar forge-bootstrap-packages
  '(all-the-icons all-the-icons-dired smart-mode-line doom-modeline rainbow-mode jabber emojify
                  paradox exec-path-from-shell
                  async
                  page-break-lines yasnippet flycheck company aggressive-indent undo-tree
                  anaconda-mode company-anaconda
                  go-mode markdown-mode web-mode php-mode ledger-mode yaml-mode json-mode
                  elfeed
                  magit magit-annex git-annex git-timemachine
                  paredit
                  gnus-alias
                  org-plus-contrib org-mime org-bullets ox-twbs ox-reveal ox-tufte org-present org-pomodoro
                  pass auth-source-pass
                  ivy swiper counsel smex ace-window avy dumb-jump hydra)
  "A list of packages that will be installed as part of bootstrap process.")

(defun forge/package-install (package)
  "Install PACKAGE if not yet installed."
  (unless (package-installed-p package)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (package-install package)
    (message "Installed package %s." package)
    (delete-other-windows)))

(defun forge/upgrade-packages ()
  "Upgrade all installed packages."
  (interactive)
  (save-window-excursion
    (package-refresh-contents)
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (package-menu-execute 'noquery)
    (message "Packages updated.")))

(defun forge/bootstrap-packages ()
  "Bootstrap packages to install for Emacs."
  (interactive)
  (dolist (package forge-bootstrap-packages)
    (progn (forge/package-install package)))
  (all-the-icons-install-fonts))

;; Via spacemacs/core/core-funcs.el
;; https://github.com/syl20bnr/spacemacs/blob/c7a103a772d808101d7635ec10f292ab9202d9ee/core/core-funcs.el
(defun forge/recompile-elpa ()
  "Recompile packages in elpa directory.  Useful if you switch Emacs versions."
  (interactive)
  (byte-recompile-directory package-user-dir nil t))

(setq package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(require 'package)
(when (version< emacs-version "27.0")
  (package-initialize))

(dolist (package forge-core-packages)
  (progn (forge/package-install package)))

;; https://github.com/jwiegley/use-package
(eval-when-compile
  (require 'use-package))

(setq use-package-verbose t
      use-package-minimum-reported-time 0.1)

(use-package diminish :demand t)
(use-package bind-key)
(require 'cl)

;;;
;;; Paradox
;;; https://github.com/Malabarba/paradox
(use-package paradox
  :defer t
  :init
  (setq paradox-execute-asynchronously t))

;;;
;;; Quelpa
;;; https://github.com/quelpa/quelpa
(use-package quelpa
  :defer t
  :init
  (setq quelpa-dir (concat forge-state-dir "quelpa")
        quelpa-checkout-melpa-p nil  ;; I'm not using quelpa for packages already in melpa
        quelpa-update-melpa-p nil))

(use-package quelpa-use-package
  :defer t
  :after quelpa)

;;;
(provide 'forge-package)
;;; forge-package.el ends here
