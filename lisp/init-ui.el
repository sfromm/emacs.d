;;; init-ui.el --- Init UI elements -*- lexical-binding: t -*-
;; UI here is rather broadly defined ... not just the classical one.
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


(defvar-keymap my-mkhome-map
  :doc "Prefix map for mkhome targets"
  "g" #'forge-mkhome-update
  "w" #'forge-mkhome-www
  "s" #'forge-mkhome-src)

(defvar-keymap my-jump-map
  :doc "Prefix map for jumping"
  "j" #'avy-goto-char-timer
  "i" #'imenu
  "d" #'dired-jump
  "m" #'consult-bookmark
  "b" #'my-open-browser-bookmark)

(defvar-keymap my-window-map
  :doc "Prefix map for managing windows and buffers"
  :name "Windows"
  "a" #'ace-window
  "t" #'transpose-frame
  "o" #'ace-delete-other-windows
  "s" #'ace-swap-window
  "d" #'ace-delete-window
  "b" #'consult-buffer
  "g" #'golden-ratio)

(defvar-keymap my-mail-search-map
  :doc "Prefix map for searching email"
  :name "Search Email"
  "a" #'notmuch-search-attachment
  "d" #'notmuch-search-last-day
  "w" #'notmuch-search-last-week
  "m" #'notmuch-search-last-month)

(defvar-keymap my-mail-map
  :doc "Prefix map for working with email"
  :name "Mail"
  "m" #'notmuch-cycle-notmuch-buffers
  "s" my-mail-search-map
  "O" #'forge/mail-org-notes
  "W" #'forge/notmuch-save-all-attachments
  "N" #'forge/mail-toggle-compose-new-frame)

(define-prefix-command 'forge-map)
(define-key forge-map (kbd "w") my-window-map)
(define-key forge-map (kbd "m") my-mail-map)
(define-key forge-map (kbd "h") my-mkhome-map)
(define-key forge-map (kbd "f") 'elfeed)
(define-key forge-map (kbd "j") my-jump-map)
(define-key forge-map (kbd "g") 'magit-status)
(define-key forge-map (kbd "s") 'eshell-here)
(define-key forge-map (kbd "S") 'forge/slack/body)
(define-key forge-map (kbd "t") 'org-pomodoro)
(define-key forge-map (kbd "p") 'package-list-packages)
(define-key forge-map (kbd "u") 'browse-url-at-point)
(define-key forge-map (kbd "V") 'view-mode)
(define-key forge-map (kbd "F") 'forge-focus)
(global-set-key (kbd "C-z") 'forge-map)


(use-package hydra
  :demand t
  :config
  (defhydra forge/navigate (:foreign-keys run)
    "[Navigate] or q to exit."
    ("a" beginning-of-line)
    ("e" end-of-line)
    ("l" forward-char)
    ("h" backward-char)
    ("n" next-line)
    ("j" next-line)
    ("p" previous-line)
    ("k" previous-line)
    ("d" View-scroll-half-page-forward)
    ("u" View-scroll-half-page-backward)
    ("SPC" scroll-up-command)
    ("S-SPC" scroll-down-command)
    ("[" backward-page)
    ("]" forward-page)
    ("{" backward-paragraph)
    ("}" forward-paragraph)
    ("<" beginning-of-buffer)
    (">" end-of-buffer)
    ("." end-of-buffer)
    ("C-'" nil)
    ("q" nil :exit t))

  (defhydra forge/music-mpd-hydra ()
    "MPD Actions"
    ("p" mingus-toggle "Play/Pause")
    ("/" mingus-search "Search" :exit t)
    ("c" (message "Currently Playing: %s" (shell-command-to-string "mpc status")) "Currently Playing")
    ("m" mingus "Mingus" :exit t)
    ("<" (progn
           (mingus-prev)
           (message "Currently Playing: %s" (shell-command-to-string "mpc status"))) "Previous")
    (">" (progn
           (mingus-next)
           (message "Currently Playing: %s" (shell-command-to-string "mpc status"))) "Next")
    ("+" (dotimes (i 5) (mingus-vol-up)) "Louder")
    ("-" (dotimes (i 5) (mingus-vol-down)) "Quieter")
    ("q" nil "Quit"))

  (defhydra forge/music-emms-hydra ()
    "EMMS Actions"
    ("SPC" emms-pause "Play/Pause")
    ("s" emms-stop "Stop")
    ("c" emms-show "Currently Playing")
    ("m" emms "EMMS")
    ("S" emms-streams "EMMS Streams")
    ("<" emms-previous "Previous")
    (">" emms-next "Next")
    ("+" emms-volume-raise "Louder")
    ("-" emms-volume-lower "Quieter")
    ("C" emms-playlist-clear "Clear")
    ("q" nil "Quit"))

  (defhydra forge/slack (:color blue)
    ("s" slack-start "Start")
    ("i" slack-im-select "IM")
    ("g" slack-group-select "Group")
    ("c" slack-channel-select "Channel")
    ("d" slack-ws-close "Disconnect")
    ("q" nil))

  )


(when (require 'tab-bar nil 'noerror)
  (tab-bar-mode)
  (setq tab-bar-close-tab-select 'recent
        tab-bar-close-button-show nil
        tab-bar-new-tab-choice 'ibuffer
        tab-bar-new-tab-to 'right
        tab-bar-position nil
        tab-bar-select-tab-modifiers '(super meta)
        tab-bar-tab-hints t
        tab-bar-show 1))

;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/prot-lisp/prot-tab.el
(defun prot-tab--tab-bar-tabs ()
  "Return a list of `tab-bar' tabs, minus the current one."
  (mapcar (lambda (tab)
            (alist-get 'name tab))
          (tab-bar--tabs-recent)))

(defun forge/switch-tab-dwim ()
  "Do-What-I-Mean (DWIM) switch to other tab.
This will create a new tab if no tabs exist, switch
to the other tab if there are only 2 tabs, and finally
prompt for what tab to switch to."
  (interactive)
  (let ((tabs (prot-tab--tab-bar-tabs)))
    (cond ((eq tabs nil)
           (tab-new))
          ((eq (length tabs) 1)
           (tab-next))
          (t
           (call-interactively #'tab-bar-select-tab-by-name)))))

(global-set-key (kbd "C-x t t") #'forge/switch-tab-dwim)

(use-package transpose-frame)

(use-package golden-ratio
  :hook
  (ediff-before-setup-windows . (lambda () (golden-ratio-mode -1)))
  :config
  (setq golden-ratio-exclude-modes '(messages-buffer-mode
                                     fundamental-mode
                                     ediff-mode
                                     calendar-mode
                                     calc-mode
                                     calc-trail-mode
                                     magit-popup-mode))
  (add-to-list 'golden-ratio-extra-commands 'ace-window))

(with-eval-after-load 'uniquify
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        uniquify-ignore-buffers-re "^\\*"
        uniquify-after-kill-buffer-p t))

(use-package olivetti
  :custom
  (olivetti-hide-mode-line t)
  (olivetti-body-width 80)
  :commands olivetti-mode
  :preface
  (defun forge-focus ()
    "Enable features to focus."
    (interactive)
    (olivetti-mode)))

(provide 'init-ui)
