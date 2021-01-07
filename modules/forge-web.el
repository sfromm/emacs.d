;;; forge-web.el -- Set up editing of web-related files.  -*- lexical-binding: t -*-

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

;;; Code:


(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" . web-mode)
  :custom
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2))


;;;
;;; restclient
;;; https://github.com/pashky/restclient.el
(use-package restclient
  :ensure t
  :defer t)

;;;
;;; request
;;; https://github.com/tkf/emacs-request/
(use-package request
  :ensure t
  :defer t)


;;;
;;; eww
;;; https://www.gnu.org/software/emacs/manual/html_mono/eww.html
(use-package eww
  :defer t
  :bind (:map eww-mode-map
              ("j" . next-line)
              ("k" . previous-line))
  :hook ((eww-mode . (lambda () (url-cookie-delete-cookies nil)))
         (eww-mode . olivetti-mode)))


;;;
;;; url-vars
(use-package url-vars
  :custom
  (url-privacy-level '(email agent cookies lastloc)))


;;;

(provide 'forge-web)
;;; forge-web.el ends here
