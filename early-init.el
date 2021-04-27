;;; early-init.el --- Early Init File -*- lexical-binding: t -*-
;; Copyright (C) 2021 by Stephen Fromm

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

(setq package-enable-at-startup t)            ;; initialize now with package-quickstart
(setq package-quickstart-file (locate-user-emacs-file "var/package-quickstart.el"))
(setq package-quickstart t)
(setq gc-cons-threshold most-positive-fixnum) ;; Set garbage collection to highest threshold
(setq message-log-max 16384)                  ;; Turn up logging settings

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
