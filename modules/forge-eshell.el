;;; forge-eshell.el --- Configure eshell.  -*- lexical-binding: t -*-

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

(use-package eshell
  :commands (eshell eshell-command)
  :preface
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

  (use-package em-unix
    :defer t
    :config
    (unintern 'eshell/su nil)
    (unintern 'eshell/sudo nil))

  :bind ("C-!" . eshell-here)

  :config
  (setenv "TERM" "xterm-256color")
  (advice-add 'eshell-life-is-too-much :after 'forge/delete-window)
  (setq tramp-default-method "ssh"
        eshell-directory-name (concat forge-state-dir "eshell")
        eshell-visual-commands '("less" "tmux" "htop" "top" "docker" "nethack")
        eshell-visual-subcommands '(("git" "log" "diff" "show"))
        eshell-prompt-function (lambda ()
                                 (concat
                                  "┌─["
                                  (user-login-name) "" (system-name)
                                  " 🗁 " (abbreviate-file-name (eshell/pwd))
                                  " 🕗 " (format-time-string "%a %b %d %H:%M" (current-time))
                                  "]\n"
                                  "└─>" (if (= (user-uid) 0) " # " " $ "))) )
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


(setq explicit-shell-file-name "/bin/bash")

(defun forge/terminal ()
  "Switch to terminal; launch if non-existent."
  (interactive)
  (if (get-buffer "*ansi-term*")
    (switch-to-buffer "*ansi-term*")
    (ansi-term "/bin/bash"))
  (get-buffer-process "*ansi-term*"))

(provide 'forge-eshell)
;;; forge-eshell.el ends here
