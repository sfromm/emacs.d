;;; init-dired.el --- Init dired integration -*- lexical-binding: t -*-
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


(with-eval-after-load 'dired
  (diminish 'dired-omit-mode)

  (define-key global-map (kbd "C-c d") 'dired-jump)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "Y") 'forge/dired-rsync)
  (define-key dired-mode-map (kbd "^") 'forge/dired-up)

  (defun forge/dired-mode-hook ()
    "Set misc settings in dired mode."
    (setq-local truncate-lines t)
    (forge/toggle-highlight-line))

  (defun forge/dired-up ()
    "Move up a directory without opening a new buffer."
    (interactive)
    (find-alternate-file ".."))

  (put 'dired-find-alternate-file 'disabled nil)
  (when (forge/system-type-darwin-p)
    (setq dired-use-ls-dired nil))

  (setq dired-dwim-target t
        dired-ls-F-marks-symlinks t
        dired-listing-switches "-laFh1v --group-directories-first"
        ;; -F (classify), -h (human readable), -1 (one file per line), -v (version sorting)
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        global-auto-revert-non-file-buffers t) ;; auto refresh dired buffers

  ;; This requires installing coreutils via homebrew
  (when (executable-find "gls")
    (setq insert-directory-program "gls"
          dired-use-ls-dired t)))

(setq auto-revert-verbose nil)

(use-package async)
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

(with-eval-after-load 'dired-x
  (setq dired-bind-jump nil
        dired-guess-shell-alist-user (list '("\\.\\(mkv\\|mpe?g\\|avi\\|mp3\\|mp4\\|ogm\\|webm\\)$" "mpv")
                                           '("\\.\\(docx?\\|xlsx?\\|kmz\\)$" "open")
                                           '("\\.pdf$" "open")))
  (global-unset-key (kbd "C-x C-j"))
  (add-to-list 'dired-omit-extensions ".DS_Store"))

(with-eval-after-load 'dired-aux
  (add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip")))

(defun forge/maybe-convert-directory-to-rsync-target (directory)
  "Adapt dired target DIRECTORY in case it is a remote target.

If directory starts with /scp: or /ssh: it is probably a tramp
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

;;(use-package emacs-conflict :commands (emacs-conflict-show-conflicts-dired))

(use-package disk-usage)
