;;; early-init.el --- Early Init File -*- lexical-binding: t -*-
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

;; native compilation
(when (and (fboundp 'native-comp-available-p) (native-comp-available-p))
  (add-to-list 'native-comp-eln-load-path (locate-user-emacs-file "var/eln"))
  (setq native-comp-deferred-compilation nil)
  (setq native-comp-async-report-warnings-errors 'silent))

;; package and package-quickstart
;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'.
(setq package-quickstart t                   ;; nope
      package-enable-at-startup t            ;; nope part deux
      package-quickstart-file (locate-user-emacs-file "var/package-quickstart.el"))

;; help startup and garbage collection
(setq gc-cons-threshold most-positive-fixnum)  ;; Set garbage collection to highest threshold
(setq message-log-max 16384)                   ;; Turn up logging settings

;; Set default coding system to UTF-8
(set-language-environment "UTF-8")

(unless (daemonp)
  (defvar init-file-name-handler-alist file-name-handler-alist)
  ;; Crank garbage collection to 11 for initialization.
  ;; Reset after init
  (setq file-name-handler-alist nil))

(defun init-reset-file-handler-alist ()
  "Reset `file-handler-alist' to initial value after startup."
  (setq file-name-handler-alist init-file-name-handler-alist))

(defun init-reset-garbage-collection ()
  "Reset garbage collection settings after startup."
  (setq gc-cons-threshold 16777216 ;; 16mb
        gc-cons-percentage 0.1
        message-log-max 1024))

(defun init-reset-startup-settings ()
  (init-reset-file-handler-alist)
  (init-reset-garbage-collection))

(add-hook 'emacs-startup-hook #'init-reset-startup-settings)
