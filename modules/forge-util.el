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
;;; MPD frontend
;;; https://github.com/pft/mingus
(use-package mingus
    :ensure t
    :defer t
    :preface
    (defun forge/get-current-song-mpd ()
      "Get the current song playing via MPD."
      (interactive)
      (let ((conn (mpd-conn-new "localhost" 6600))
            (cursong nil))
        (condition-case nil
            (setq cursong (split-string (plist-get (mpd-get-current-song conn) 'Title) " - "))
          (error nil))
        cursong)))

(defhydra forge/music-hydra ()
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


;;;
;;; Twitter
;;; Don't install by default, but provide a configuration.
(use-package twittering-mode
    :defer t
    :commands twit
    :bind
    (:map twittering-mode-map
          ("<" . twittering-goto-first-status)  ;; go to the most recent
          (">" . twittering-goto-last-status))  ;; go to the oldest
    :config
    (setq twittering-use-master-password t
          twittering-icon-mode nil
          twittering-use-icon-storage t
          twittering-icon-storage-file (concat forge-state-dir "twittering/icons.gz")
          twittering-user-id-db-file (concat forge-state-dir "twittering/user-id-info.gz")
          twittering-private-info-file (concat forge-state-dir "twittering/private.gpg")))

;;;
;;;
;;;
(provide 'forge-util)
;;; forge-util.el ends here
