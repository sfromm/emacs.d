;;; forge-core.el --- Set up the core -*- lexical-binding: t; -*-

;; Copyright (C) 2018 by Stephen Fromm

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

(defvar forge-state-dir (concat forge-emacs-dir "var/")
  "Path to Emacs' persistent data files.")

(defvar forge-backup-dir (concat forge-state-dir "backup/")
  "Path to Emacs' backup and autosave files.")

(add-to-list 'load-path forge-modules-dir)
(add-to-list 'load-path forge-site-dir)

;; move custom out of init.el
(setq custom-file (concat forge-personal-dir "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))


;;;
;;; Packages
;;;
(setq package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(require 'package)

(defun forge/package-install (package)
  "Install PACKAGE if not yet installed."
  (unless (package-installed-p package)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (package-install package)
    (delete-other-windows)))

(forge/package-install 'use-package)
(require 'use-package)
(require 'cl)

;;;
;;; Paradox
;;;
(use-package paradox
  :ensure t
  :init
  (setq paradox-execute-asynchronously t))

;;;
;;; Platform specific
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

;;;
;;; Platform
;;; Maybe one day spin this out to separate file.
;;;
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

;;;
;;; Backups and auto-save
;;;
(defun forge/save-all ()
  "Save any file-related buffers."
  (interactive)
  (message "Saving buffers at %s" (format-time-string "%Y-%m-%dT%T"))
  (save-some-buffers t))

(setq
  backup-directory-alist `((".*" forge-backup-dir))
  auto-save-file-name-transforms `((".*" forge-backup-dir t))
  delete-old-versions -1
  version-control t
  savehist-file (concat forge-state-dir "savehist")
  savehist-save-minibuffer-history 1
  savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
  auto-save-timeout 120
  auto-save-interval 1000)

(savehist-mode 1)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; If focus switches away, save all files.
(when (version<= "24.4" emacs-version)
  (add-hook 'focus-out-hook 'forge/save-all))

;;;
(provide 'forge-core)
;;; forge-core.el ends here
