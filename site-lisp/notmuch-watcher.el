;;; notmuch-watcher.el --- Run and watch a notmuch process. -*- lexical-binding: t -*-

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

;; This will start a `notmuch-watcher-command' process in a process buffer.
;; This will typically be something like 'notmuch new'.  Then,
;; per the refresh inteval configured with `notmuch-watcher-refresh-interval',
;; the notmuch-watcher command will be run again.

;; If you have something that loops and does not terminate, then set
;; `notmuch-watcher-refresh-interval' to 0.  This will run the normal command
;; to run notmuch.

;; It is generally assumed you are using the notmuch hooks.  The `pre-new' hook
;; would have something to download email via either mbsync of offlineimap and
;; the `post-new' would have something to tag messages acording to your rules.

;;; Code:

(require 'comint)

(defgroup notmuch-watcher nil
  "Notmuch watcher."
  :group 'notmuch)

(defcustom notmuch-watcher-buffer-name "*notmuch-watcher*"
  "Name of buffer to run notmuch new."
  :group 'notmuch-watcher
  :type 'string)

(defcustom notmuch-watcher-command "notmuch new"
  "Command to run."
  :group 'notmuch-watcher
  :type 'string)

(defcustom notmuch-watcher-refresh-interval 180
  "Interval to re-run notmuch new."
  :group 'notmuch-watcher
  :type 'integer)

(defcustom notmuch-watcher-buffer-maximum-size comint-buffer-maximum-size
  "The maximum size in lines for notmuch buffer."
  :group 'notmuch-watcher
  :type 'integer)

(defvar notmuch-watcher-refresh-timer nil
  "Variable to track the update timer for notmuch new.")

(defvar notmuch-watcher-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'notmuch-watcher-quit)
    (define-key map (kbd "g") 'notmuch-watcher-refresh)
    (define-key map (kbd "<") 'notmuch-watcher-first)
    (define-key map (kbd ">") 'notmuch-watcher-last)
    map)
  "Keymap for notmuch-new-mode.")

(defun notmuch-watcher-make-buffer ()
  "Get the notmuch-new buffer."
  (let ((buffer (get-buffer-create notmuch-watcher-buffer-name)))
    (with-current-buffer buffer
      (notmuch-watcher-mode))
    buffer))

(define-derived-mode notmuch-watcher-mode fundamental-mode "NotmuchWatcher"
                     "A major mode for notmuch interaction."
                     :group 'notmuch
                     (setq buffer-read-only t)
                     (setq buffer-undo-list t)
                     (when (> notmuch-watcher-refresh-interval 0)
                       (setq notmuch-watcher-refresh-timer
                             (run-at-time 60 notmuch-watcher-refresh-interval 'notmuch-watcher-refresh) )))

(defun notmuch-watcher-insert (process text)
  "Insert TEXT in PROCESS buffer."
  (let ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (if (eq (point) (point-max))
              (progn
                (insert (concat text "\n"))
                (set-marker (process-mark process) (point)))
            (save-excursion
              (goto-char (point-max))
              (insert (concat text "\n"))
              (set-marker (process-mark process) (point)))))))))

(defun notmuch-watcher-process-filter (process msg)
  "Filter PROCESS output MSG."
  (dolist (msg-line (nbutlast (split-string msg "[\n\r]+")))
    (let* ((buffer (process-buffer process)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (unless (string= "" msg-line)
            (notmuch-watcher-insert process msg-line)))))))

(defun notmuch-watcher-process-running-p ()
  "Return non-nil if PROCESS is running."
  (eq (process-status (notmuch-watcher-get-process)) 'run))

(defun notmuch-watcher-process-exit-p ()
  "Return non-nil if PROCESS has exited."
  (eq (process-status (notmuch-watcher-get-process)) 'exit))

(defun notmuch-watcher-process-sentinel (process state)
  "Monitor STATE change of PROCESS."
  (when (notmuch-watcher-process-exit-p)
    (notmuch-watcher-insert process "notmuch new exited.")))

(defun notmuch-watcher-get-process ()
  "Get the running notmuch new process (or nil if no such)."
  (let ((buffer (get-buffer notmuch-watcher-buffer-name)))
    (and (buffer-live-p buffer)
         (get-buffer-process buffer))))

(defun notmuch-watcher (&optional nada)
  "Run `notmuch-watcher-command'."
  (interactive "P")
  (let* ((buffer (notmuch-watcher-make-buffer)))
    (unless (notmuch-watcher-get-process)
      (let ((process (start-process-shell-command
                      notmuch-watcher-buffer-name
                      buffer
                      notmuch-watcher-command)))
        (set-process-filter process 'notmuch-watcher-process-filter)
        (set-process-sentinel process 'notmuch-watcher-process-sentinel)))))

(defun notmuch-watcher-first ()
  "Go to beginning of notmuch-watcher buffer."
  (interactive)
  (goto-char (point-min)))

(defun notmuch-watcher-last ()
  "Go to end of notmuch-watcher buffer."
  (interactive)
  (goto-char (point-max)))

(defun notmuch-watcher-quit ()
  "Quit notmuch new."
  (interactive)
  (when notmuch-watcher-refresh-timer
    (cancel-timer notmuch-watcher-refresh-timer))
  (kill-buffer (get-buffer notmuch-watcher-buffer-name)))

(defun notmuch-watcher-refresh ()
  "Re-run notmuch new."
  (interactive)
  (unless (notmuch-watcher-get-process)
    (notmuch-watcher)))

(provide 'notmuch-watcher)
;;; notmuch-watcher.el ends here
