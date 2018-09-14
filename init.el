;;; init.el -- Forge configuration initialization

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
;; Load up the forge modules.

;;; Code:

(message "Loading up Emacs...")

(require 'forge-core (concat user-emacs-directory "modules/forge-core"))

(forge/load-modules 'forge-appearance
                    'forge-editing
                    'forge-ui
                    'forge-dired
                    'forge-git
                    'forge-eshell
                    'forge-dired
                    'forge-markdown
                    'forge-orgmode
                    'forge-pass
                    'forge-web)

(message "Emacs is ready, finished loading after %.03fs."
  (float-time (time-subtract after-init-time before-init-time)))

;;; init.el ends here
