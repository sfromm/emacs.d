;;; init-term.el --- Init Shell -*- lexical-binding: t -*-
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


(with-eval-after-load 'em-unix
  (unintern 'eshell/su nil)
  (unintern 'eshell/sudo nil))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the current buffer's file."
  (interactive)
  (let* ((parent (if (buffer-file-name) (file-name-directory (buffer-file-name)) (getenv "HOME")))
         (height (/ (window-total-height) 3))
         (name (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(define-key global-map (kbd "C-!") 'eshell-here)

(with-eval-after-load 'eshell

  (defun my-tramp-loaded-p ()
    "Return t if tramp is loaded and nil otherwise."
    (fboundp 'tramp-tramp-file-p))

  (defun my-eshell-tramp-path-p ()
    "Return t if path has tramp file syntax."
    (when (my-tramp-loaded-p)
      (tramp-tramp-file-p default-directory)))

  (defun my-eshell-prompt-user ()
    "Return username on current system for use in eshell prompt."
    (if (my-eshell-tramp-path-p)
        (or (tramp-file-name-user (tramp-dissect-file-name default-directory)) (getenv "USER"))
      (user-login-name)))

  (defun my-eshell-prompt-host ()
    "Return hostname of current system for use in eshell prompt."
    (if (my-eshell-tramp-path-p)
        (tramp-file-name-host (tramp-dissect-file-name default-directory))
      (system-name)))

  (defun my-eshell-prompt-pwd ()
    "Return PWD on current system for use in eshell prompt."
    (abbreviate-file-name
     (if (my-eshell-tramp-path-p)
         (nth 6 (tramp-dissect-file-name default-directory))
       (eshell/pwd))))

  (defun my-eshell-default-prompt ()
    "Generate prompt string for eshell.  Use for `eshell-prompt-function'."
    (let ((user (my-eshell-prompt-user))
          (host (my-eshell-prompt-host))
          (now (format-time-string "%b %d %H:%M" (current-time)))
          (pwd (my-eshell-prompt-pwd)))
      (concat
       "┌─[" user "  " host " " pwd "]─[" now "]\n"
       "└─>"
       (propertize " λ" 'face (if (zerop eshell-last-command-status) 'success 'error))
       " ")))

  (setenv "TERM" "xterm-256color")
  (setq explicit-shell-file-name "/bin/bash") ;; this is from term.el
  (advice-add 'eshell-life-is-too-much :after 'forge/delete-window)
  (setq tramp-default-method "ssh"
        eshell-directory-name (expand-file-name "eshell" forge-state-dir)
        eshell-visual-commands '("less" "tmux" "htop" "top" "docker" "nethack")
        eshell-visual-subcommands '(("git" "log" "diff" "show"))
        eshell-prompt-regexp "^[^#\nλ]*[#$λ] "
        eshell-prompt-function #'my-eshell-default-prompt)
  (add-hook 'eshell-mode-hook (lambda ()
                                (eshell/alias "q" "exit")
                                (eshell/alias "l" "ls -al")
                                (eshell/alias "ll" "ls -al")
                                (eshell/alias "e" "find-file \$1")
                                (eshell/alias "ff" "find-file \$1")
                                (eshell/alias "vi" "find-file \$1")
                                (eshell/alias "d" "dired \$1")
                                (eshell/alias "ee" "find-file-other-window \$1")
                                (eshell/alias "gd" "magit-diff-unstaged")
                                (eshell/alias "gds" "magit-diff-staged")
                                (eshell/alias "gst" "magit-status"))))

(defun my-terminal ()
  "Switch to terminal; launch if non-existent."
  (interactive)
  (if (get-buffer "*ansi-term*")
    (switch-to-buffer "*ansi-term*")
    (ansi-term "/bin/bash"))
  (get-buffer-process "*ansi-term*"))

(provide 'init-term)
