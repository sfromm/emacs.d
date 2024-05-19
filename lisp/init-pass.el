;;; init-pass.el --- Init password management -*- lexical-binding: t -*-
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


(when (forge/system-type-darwin-p)
  (setopt epg-gpg-program (executable-find "gpg"))
  (setopt epg-pinentry-mode 'loopback))

(use-package auth-source)

(use-package auth-source-pass
  :after auth-source
  :init
  (setq auth-sources '(password-store "~/.authinfo.gpg")))

;; https://github.com/ccrusius/auth-source-xoauth2
(use-package auth-source-xoauth2
  :after auth-source)

;;
(use-package pass
  :config
  (advice-add 'pass-quit :after 'forge/delete-window))

;; https://github.com/ecraven/ivy-pass
(use-package ivy-pass
  :bind
  ("C-c p" . ivy-pass))

(provide 'init-pass)
