;;; forge-util.el --- This is an Emacs Lisp file with Emacs Lisp code. -*- lexical-binding: t -*-

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


;;;
;;; Epub reader
;;; https://github.com/wasamasa/nov.el
(use-package nov
    :ensure t
    :mode ("\\.epub\\'" . nov-mode)
    :init
    (setq nov-save-place-file (concat forge-state-dir "nov-places")))

;;;
;;;
;;;
(provide 'forge-util)
;;; forge-util.el ends here
