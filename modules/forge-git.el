;;; forge-git.el --- Configure git and magit.  -*- lexical-binding: t -*-

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

(defvar forge-magit-repository-directories nil
  "Directories with Git repositories.  See `magit-repository-directories' for more information.")

(use-package magit
    :ensure t
    :commands magit-status
    :bind ("C-x g" . magit-status)
    :init
    (setq magit-push-always-verify nil
          magit-completing-read-function 'ivy-completing-read
          magit-repository-directories forge-magit-repository-directories
          magit-last-seen-setup-instructions "1.4.0"))


(use-package magit-annex :defer t)

(use-package git-annex
  :defer 1
  :after dired)

(use-package git-timemachine :defer t)

(provide 'forge-git)
;;; forge-git.el ends here
