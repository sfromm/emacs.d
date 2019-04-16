;;; forge-pass.el --- Configure pass, password-store, and auth-source.  -*- lexical-binding: t -*-

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

(when (forge/system-type-darwin-p)
  (custom-set-variables '(epg-gpg-program "/usr/local/bin/gpg"))
  (setq epa-pinentry-mode 'loopback))

(use-package auth-source)

(use-package pass
  :ensure t
  :bind
  ("C-c p" . pass))

(use-package auth-source-pass
    :ensure t
    :after auth-source
    :init
    (setq auth-sources '(password-store "~/.authinfo.gpg")))

(provide 'forge-pass)
;;; forge-pass.el ends here
