;;; init.el -- Forge configuration initialization

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
;; Load up the forge modules.

;;; Code:

(message "Loading up Emacs...")

(defvar forge--core-start-time (current-time))
(require 'forge-core (concat user-emacs-directory "modules/forge-core"))
(forge/message-module-load "forge-core" forge--core-start-time)


;;;
;;; Load any site lisp files.
;;;
(forge/load-directory-modules forge-site-dir)


;;;
;;; Load forge modules.
;;;
(forge/load-modules 'forge-appearance
                    'forge-editing
                    'forge-ui
                    'forge-pass
                    'forge-git
                    'forge-eshell
                    'forge-dired
                    'forge-markdown
                    'forge-orgmode
                    'forge-mail
                    'forge-chat
                    'forge-util
                    'forge-elfeed)


;;;
;;; Load any personal lisp files, including `custom-file'.
;;;
(setq custom-file (concat forge-personal-dir "custom.el"))

(forge/load-directory-modules forge-personal-dir)

(message "Emacs is ready, finished loading after %.03fs."
         (float-time (time-subtract after-init-time before-init-time)))

;;; init.el ends here
