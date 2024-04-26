;;; init.el --- Init File -*- lexical-binding: t -*-

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

(message "Loading up Emacs...")
(defvar init-core-start-time (current-time))

(defun init-report-startup-time ()
  "Report startup time."
  (interactive)
  (message "Emacs is ready, finished loading after %.03fs."
           (float-time (time-subtract after-init-time before-init-time))))

(add-hook 'after-init-hook #'init-report-startup-time)

(require 'init-elpa)


(defvar forge-site-dir (expand-file-name "site-lisp/" user-emacs-directory)
  "Path to user's site configuration.")

(defvar forge-personal-dir (expand-file-name "user/" user-emacs-directory)
  "Path to user's personal configuration.")

(defvar forge-themes-dir (expand-file-name "themes/" user-emacs-directory)
  "Path to user themes.")

(defvar forge-state-dir (expand-file-name "var/" user-emacs-directory)
  "Path to Emacs' persistent data files.")

(defvar forge-backup-dir (expand-file-name "backup/" forge-state-dir)
  "Path to Emacs' backup and autosave files.")

(defvar forge-log-dir (expand-file-name "log/" forge-state-dir)
  "Path to Emacs packages' log files.")


;; Load custom and then do basic initialization.
(setq custom-file (expand-file-name "custom.el" forge-personal-dir))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'init-core)

(require 'init-appearance)

(require 'init-ui-completion)

(require 'init-navigation)

(require 'init-ui)

(require 'init-ibuffer)

(require 'init-editing)

(require 'init-editing-lang)


;; delete-trailing-whitespace
(let ((hooks '(csv-mode-hook json-mode-hook prog-mode-hook yaml-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'delete-trailing-whitespace)) hooks))

(require 'init-chat)

(require 'notifications)
(require 'tls)

(require 'init-dired)

(require 'init-git)

(require 'init-email)

(require 'init-org)

(require 'init-pass)

(require 'init-elfeed)

(require 'init-term)

(require 'init-utils)

(forge/load-directory-modules forge-personal-dir)
