;;; forge-core.el --- Set up the core.  -*- lexical-binding: t -*-

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

(defvar forge-emacs-dir (file-truename user-emacs-directory)
  "Path to this emacs.d directory.")

(defvar forge-modules-dir (concat forge-emacs-dir "modules/")
  "Path to built-in modules.")

(defvar forge-site-dir (concat forge-emacs-dir "site/")
  "Path to user's site configuration.")

(defvar forge-personal-dir (concat forge-emacs-dir "user/")
  "Path to user's personal configuration.")

(defvar forge-preload-dir (concat forge-emacs-dir "preload/")
  "Path to user's personal configuration to preload before anything else.")

(defvar forge-state-dir (concat forge-emacs-dir "var/")
  "Path to Emacs' persistent data files.")

(defvar forge-backup-dir (concat forge-state-dir "backup/")
  "Path to Emacs' backup and autosave files.")

(defvar forge-log-dir (concat forge-state-dir "log/")
  "Path to Emacs packages' log files.")

(add-to-list 'load-path forge-modules-dir)
(add-to-list 'load-path forge-site-dir)


;;;
;;; Packages
;;;
(setq package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(when (version< emacs-version "27.0")
  (package-initialize))
(require 'package)

(defun forge/package-install (package)
  "Install PACKAGE if not yet installed."
  (unless (package-installed-p package)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (package-install package)
    (delete-other-windows)))

(defun forge/update-packages ()
  "Update all installed packages."
  (interactive)
  (save-window-excursion
    (package-refresh-contents)
    (paradox-upgrade-packages)
    (message "Packages updated.")))

(defun forge/bootstrap-packages ()
  "Bootstrap packages to install for Emacs."
  (interactive)
  (dolist (package '(all-the-icons all-the-icons-dired smart-mode-line doom-modeline rainbow-mode
                     jabber
                     paradox exec-path-from-shell
                     async
                     page-break-lines yasnippet flycheck aggressive-indent markdown-mode web-mode yaml-mode json-mode
                     elfeed
                     magit magit-annex git-annex git-timemachine
                     paredit
                     gnus-alias
                     org-plus-contrib org-mime org-bullets ox-twbs ox-reveal ox-tufte org-present org-pomodoro
                     pass auth-source-pass
                     ivy swiper counsel smex ace-window avy dumb-jump hydra))
    (progn (forge/package-install package))))

(forge/package-install 'use-package)
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
(require 'cl)

;;;
;;; Paradox
;;;
(use-package paradox
  :ensure t
  :init
  (setq paradox-execute-asynchronously t))


;;;
;;; Helper functions
;;;
(defun forge/turn-on-hl-line ()
  "Turn on `hl-line-mode'."
  (interactive)
  (hl-line-mode 1))

(defun forge/turn-off-hl-line ()
  "Turn off `hl-line-mode'."
  (interactive)
  (hl-line-mode nil))

(defun forge/turn-on-delete-trailing-whitespace ()
  "Turn on `delete-trailing-whitespace' when saving files."
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))

(defun forge/turn-off-delete-trailing-whitespace ()
  "Turn off `delete-trailing-whitespace' when saving files."
  (remove-hook 'before-save-hook 'delete-trailing-whitespace t))

(defun forge-initialize ()
  "Initialize paths and session for this Emacs instance."
  (dolist (dir (list forge-site-dir forge-personal-dir forge-state-dir forge-backup-dir forge-log-dir forge-preload-dir))
    (unless (file-directory-p dir)
      (make-directory dir t)))
  (setq inhibit-splash-screen t
        ;; always load the newer version of a file
        load-prefer-newer t
        ;; only run garbage collection after 50MB of allocated data.
        gc-cons-threshold 50000000
        ;; warn when opening files bigger than 50MB
        large-file-warning-threshold 50000000))

(defun forge/load-directory-modules (path)
  "Load Lisp files in PATH directory."
  (when (file-exists-p path)
    (message "Loading lisp files in %s..." path)
    (mapc 'load (directory-files path 't "^[^#\.].*el$"))))

(defun forge/load-modules (&rest modules)
  "Load forge modules MODULES."
  (interactive)
  (dolist (module (cons '() modules ))
    (when module
      (unless (featurep module)
        (message "Loading %s" module)
        (require module nil t)))))


;;;
;;; Platform specific details.
;;;
(defun forge/system-type-is-darwin ()
  "Return non-nil if system is Darwin/MacOS."
  (string-equal system-type "darwin"))

(defun forge/system-type-is-windows ()
  "Return non-nil if system is Windows."
  (string-equal system-type "windows-nt"))

(defun forge/system-type-is-linux ()
  "Return non-nil if system is GNU/Linux."
  (string-equal system-type "gnu/linux"))

;;;
;;; exec-path-from-shell
;;; Set exec-path based on shell PATH.
;;; Some platforms, such as MacOSX, do not get this done correctly.
;;;
(use-package exec-path-from-shell
    :ensure t
    :defer t
    :init
    (exec-path-from-shell-initialize))

(when (forge/system-type-is-linux)
  (require 'dbus))

(when (forge/system-type-is-darwin)
  (dolist (path (list "/usr/local/bin" (expand-file-name "~/bin")))
    (progn
      (setenv "PATH" (concat path ":" (getenv "PATH")))
      (add-to-list 'exec-path path))))


;;;
;;; Set up the server
;;;
(load "server")
(unless (server-running-p) (server-start))
(forge-initialize)

;;;
(provide 'forge-core)
;;; forge-core.el ends here
