;;; forge-dired.el --- Configure dired.  -*- lexical-binding: t -*-

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


(use-package dired
    :defer t
    :preface
    (defun forge/dired-mode-hook ()
      "Set misc settings in dired mode."
      (setq-local truncate-lines t)
      (forge/turn-on-hl-line))

    (defun forge/dired-up ()
      "Move up a directory without opening a new buffer."
      (interactive)
      (find-alternate-file ".."))

    :bind
    (("C-c d" . dired-jump)
     :map dired-mode-map
     ("RET" . dired-find-alternate-file)
     ("Y" . forge/dired-rsync)
     ("^" . forge/dired-up))

    :diminish dired-omit-mode

    :config
    (put 'dired-find-alternate-file 'disabled nil)
    (when (forge/system-type-darwin-p)
      (setq dired-use-ls-dired nil)

      ;; This requires installing coreutils via homebrew
      (when (executable-find "gls")
        (setq insert-directory-program "gls")))

    (setq dired-dwim-target t
          dired-ls-F-marks-symlinks t
          dired-recursive-copies 'always
          dired-recursive-deletes 'top
          global-auto-revert-non-file-buffers t ;; auto refresh dired buffers
          auto-revert-verbose nil))


;;;
;;; dired-x
;;;
(use-package dired-x
    :after dired
    :init
    (global-unset-key (kbd "C-x C-j"))
    (setq dired-bind-jump nil)
    :config
    (add-to-list 'dired-omit-extensions ".DS_Store"))

(use-package async :ensure t)
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)


;;;
;;; Dired Rsync
;;;
;;; https://github.com/tmtxt/tmtxt-dired-async/pull/6/files
(defun forge/maybe-convert-directory-to-rsync-target (directory)
  "If directory starts with /scp: or /ssh: it is probably a tramp
target and should be converted to rsync-compatible destination
string, else we do (shell-quote-argument (expand-file-name
directory)) as is required for normal local targets acquired with
read-file-name and dired-dwim-target-directory."
  (if (or (string-prefix-p "/scp:" directory)
	  (string-prefix-p "/ssh:" directory))
      ;; - throw out the initial "/scp:" or "/ssh:"
      ;; - replace spaces with escaped spaces
      ;; - surround the whole thing with quotes
      ;; TODO: double-check that target ends with "/""
      ;; which in the case of DWIM is what we want
      (prin1-to-string
       (replace-regexp-in-string "[[:space:]]" "\\\\\\&"
	                         (substring directory 5)))
    (shell-quote-argument (expand-file-name directory))))

(defun forge/dired-rsync (dest)
  (interactive
   (list
    (expand-file-name (read-file-name "Rsync to:" (dired-dwim-target-directory)))))
  ;; store all selected files into "files" list
  (let ((files (dired-get-marked-files nil current-prefix-arg))
        ;; the rsync command
        (forge/rsync-command "rsync -arvz --progress "))
    ;; add all selected file names as arguments
    ;; to the rsync command
    (dolist (file files)
      (setq forge/rsync-command (concat forge/rsync-command (shell-quote-argument file) " ")))
    ;; append the destination
    (setq forge/rsync-command (concat forge/rsync-command (forge/maybe-convert-directory-to-rsync-target dest)))
    ;; run the async shell command
    (async-shell-command forge/rsync-command "*rsync*")
    ;; finally, switch to that window
    (other-window 1)))

;;;
;;;
;;;
(provide 'forge-dired)
;;; forge-dired.el ends here
