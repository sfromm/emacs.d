;;; notmuch-speedbar --- Speedbar support for notmuch

;; Copyright (C) 2012-2018 Stephen Fromm
;;
;; Author: Stephen Fromm
;; Version: 0.1
;; Keywords: file, tags, tools
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Speedbar provides a frame in which files, and locations in files
;; are displayed.  These functions provide notmuch specific support,
;; showing tags in the side-bar.
;;
;;   This file requires speedbar.

;;; Code:

(require 'speedbar)
(require 'notmuch-lib)
(require 'notmuch-hello)

(defvar notmuch-speedbar-key-map nil
  "Keymap used when in notmuch speedbar mode.")

(defun notmuch-speedbar-insert-saved-searches ()
  "Insert the saved-searches into speedbar."
  (let ((searches (notmuch-hello-query-counts
                   (if notmuch-saved-search-sort-function
                       (funcall notmuch-saved-search-sort-function notmuch-saved-searches)
                     notmuch-saved-searches)
                   :show-empty-searches notmuch-show-empty-saved-searches)))
    (when searches
      (let ((start (point)))
        (notmuch-hello-insert-buttons searches)
        (indent-rigidly start (point) notmuch-hello-indent)))))

(defun notmuch-speedbar-install-variables ()
  "Install those variables used by speedbar to enhance notmuch."
  (add-hook 'notmuch-search-hook
            (lambda()
              (when (buffer-live-p speedbar-buffer)
                (with-current-buffer speedbar-buffer
                  (let ((inhibit-read-only t))
                    (notmuch-speedbar-buttons))))))
  (dolist (keymap '(notmuch-speedbar-key-map))
    (unless keymap
      (setq keymap (speedbar-make-specialized-keymap))
      (define-key keymap "RET" 'speedbar-edit-line)
      (define-key keymap "e" 'speedbar-edit-line))))

(if (featurep 'speedbar)
    (notmuch-speedbar-install-variables)
  (add-hook 'speedbar-load-hook 'notmuch-speedbar-install-variables))

;;;###autoload
(defun notmuch-speedbar-buttons (&optional buffer)
  "Create buttons for any notmuch BUFFER."
  (interactive)
  (erase-buffer)
  (insert (propertize "Notmuch Saved searches\n" 'face 'notmuch-tag-face))
  (notmuch-speedbar-insert-saved-searches))
;;  (insert "\n")
;;  (insert (propertize " All tags\n" 'face 'notmuch-tag-face))
;;  (notmuch-hello-insert-alltags))

(provide 'notmuch-speedbar)
;;; notmuch-speedbar.el ends here
