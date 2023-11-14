;;; init-elpa.el --- Init ELPA and package management -*- lexical-binding: t -*-
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


(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(defun my-package-install (package)
  "Install PACKAGE if not yet installed."
  (unless (fboundp 'package-installed-p)
    (package-initialize))
  (unless (package-installed-p package)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (package-install package)
    (message "Installed package %s." package)
    (delete-other-windows)))

(defun my-package-upgrade-packages ()
  "Upgrade all installed packages."
  (interactive)
  (save-window-excursion
    (package-refresh-contents)
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (package-menu-execute 'noquery)
    (message "Packages updated.")))

;; Via spacemacs/core/core-funcs.el
;; https://github.com/syl20bnr/spacemacs/blob/c7a103a772d808101d7635ec10f292ab9202d9ee/core/core-funcs.el
(defun my-recompile-elpa ()
  "Recompile packages in elpa directory.  Useful if you switch Emacs versions."
  (interactive)
  (byte-recompile-directory package-user-dir nil t))


(defvar init-core-packages '(use-package diminish org org-contrib)
  "A list of core packages that will be automatically installed.")

(defun init-install-core-packages ()
  "Install core packages to bootstrap Emacs environment."
  (interactive)
  (dolist (package init-core-packages)
    (progn (my-package-install package))))

(init-install-core-packages)

;; https://github.com/jwiegley/use-package
(eval-when-compile
  (require 'diminish)
  (require 'use-package)
  (require 'use-package-ensure))

(setq use-package-verbose t
      use-package-compute-statistics t       ;; compute stats
      use-package-always-ensure t
      use-package-minimum-reported-time 0.1) ;; carp if it takes awhile to load a package

(cl-defun init-vc-install (&key (fetcher "github") repo name rev backend)
  "Install a package from a remote.
This is meant to be a thin wrapper around `package-vc-install'.  Takes
the following arguments:

- FETCHER the remote where to get the package (e.g. \"gitlab\").
  Defaults to \"github\".
- REPO is the name of the repository (e.g. \"sfromm/ip-query\").
- NAME, REV, and BACKEND are passed to `package-vc-install'."
  (interactive)
  (let* ((url (format "https://www.%s.com/%s" fetcher repo))
         (iname (when name (intern name)))
         (pkg (or iname (intern (file-name-base repo)))))
    (unless (package-installed-p pkg)
      (package-vc-install url iname rev backend))))


(use-package paradox
  :init
  (setq paradox-execute-asynchronously t))

(provide 'init-elpa)
