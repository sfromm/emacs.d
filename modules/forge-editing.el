;;; forge-editing.el --- This is an Emacs Lisp file with Emacs Lisp code.  -*- lexical-binding: t -*-

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

;;; Code:

;;;
;;; yasnippets
;;;
(use-package yasnippet
  :ensure t
  :diminish yasnippet-minor-mode
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (concat forge-user-dir "snippets"))
  (add-hook 'term-mode-hook (lambda () "Disable yasnippet in terminal" (setq yas-dont-activate t))))

;;;
;;; Fill paragraph ... unfill
;;; From http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph] #'endless/fill-or-unfill)

;;;
;;; Backups and auto-save
;;;
(defun forge/save-all ()
  "Save any file-related buffers."
  (interactive)
  (message "Saving buffers at %s" (format-time-string "%Y-%m-%dT%T"))
  (save-some-buffers t))

(setq backup-directory-alist (list (cons ".*" forge-backup-dir)) ;; make backups of files to the backup directory
      auto-save-file-name-transforms `((".*" ,forge-backup-dir t))   ;;
      delete-old-versions -1
      version-control t
      savehist-file (concat forge-state-dir "savehist")
      savehist-save-minibuffer-history 1
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      auto-save-timeout 120
      auto-save-interval 1000)

(savehist-mode 1)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; If focus switches away, save all files.
(when (version<= "24.4" emacs-version)
  (add-hook 'focus-out-hook 'forge/save-all))

(provide 'forge-editing)
;;; forge-editing.el ends here
