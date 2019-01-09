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

(use-package magit
    :ensure t
    :init
    (progn
      (setq magit-push-always-verify nil
	    magit-completing-read-function 'ivy-completing-read
	    magit-last-seen-setup-instructions "1.4.0"))
    :commands magit-status
    :bind ("C-x g" . magit-status))

(use-package magit-annex :ensure t)

(use-package git-annex :ensure t)

(use-package git-timemachine :ensure t)

(provide 'forge-git)
;;; forge-git.el ends here
