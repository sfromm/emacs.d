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
  :disabled t
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

;;;
;;; EMMS - Emacs Multimedia System
;;; https://www.gnu.org/software/emms/
;;; https://www.gnu.org/software/emms/manual/
(use-package emms
  :ensure t
  :defer t
  :config
  (emms-all)
  (emms-history-load)
  (setq emms-directory (concat forge-state-dir "emms")
        emms-player-list (list emms-player-mpv)
        emms-stream-info-backend 'mplayer
        emms-source-file-default-directory (expand-file-name "~/annex/Audio")
        emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find
        emms-browser-covers 'emms-browser-cache-thumbnail)
  (add-to-list 'emms-player-mpv-parameters "--no-audio-display")
  (add-to-list 'emms-info-functions 'emms-info-cueinfo)
  (if (executable-find "emms-print-metadata")
      (progn
        (require 'emms-info-libtag)
        (add-to-list 'emms-info-functions 'emms-info-libtag)
        (delete 'emms-info-ogginfo emms-info-functions)
        (delete 'emms-info-mp3info emms-info-functions))
    (add-to-list 'emms-info-functions 'emms-info-ogginfo)
    (add-to-list 'emms-info-functions 'emms-info-mp3info)))

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



;;;
;;; Twitter
;;; Don't install by default, but provide a configuration.
;;; The org capture and integration is from:
;;;  https://github.com/hayamiz/twittering-mode/pull/81
;;;  https://gist.github.com/danieroux/7838667
(use-package twittering-mode
  :defer t
  :commands twit
  :preface
  (defun forge/twittering-toggle-icons ()
    "Toggle use of icons in twittering mode."
    (interactive)
    (if (eq twittering-icon-mode t)
        (twittering-icon-mode nil)
      (twittering-icon-mode t)))

  (defun org-twittering-open (id-str)
    (twittering-visit-timeline (concat ":single/" id-str)))

  (defun org-twittering-store-link ()
    "Store a link to a tweet."
    (when (and (twittering-buffer-p) (twittering-get-id-at))
      (let ((status (twittering-find-status (twittering-get-id-at))))
        (apply 'org-store-link-props
               :type "twittering"
               :link (concat "twittering:"
                             (or (cdr (assq 'retweeting-id status))
                                 (cdr (assq 'id status))))
               :description (format "@%s: %s"
                                    (cdr (assq 'user-screen-name status))
                                    (cdr (assq 'text status)))
               :url (twittering-get-status-url-from-alist status)
               :date
               (format-time-string (org-time-stamp-format)
                                   (cdr (assq 'created-at status)))
               :date-timestamp
               (format-time-string (org-time-stamp-format t)
                                   (cdr (assq 'created-at status)))
               (apply 'append
                      (mapcar
                       (lambda (sym)
                         (let ((name (symbol-name sym)))
                           `(,(intern (concat ":" name))
                             ,(or (cdr (assq sym status))
                                  (concat "[no " name "]")))))
                       '(text
                         id
                         user-id user-name user-screen-name user-description
                         user-url user-location
                         source source-url
                         retweeting-user-id retweeting-user-name
                         retweeting-user-screen-name
                         retweeting-user-description
                         retweeting-user-url
                         retweeting-user-location
                         retweeting-source retweeting-source-url)))))))

  :bind (:map twittering-mode-map
              ("I" . forge/twittering-toggle-icons)
              ("<" . twittering-goto-first-status)  ;; go to the most recent
              (">" . twittering-goto-last-status))  ;; go to the oldest
  :config
  (org-link-set-parameters "twittering"
                           :follow #'org-twittering-open
                           :store #'org-twittering-store-link)
  (setq twittering-use-master-password t
        twittering-icon-mode t
        twittering-use-icon-storage t
        twittering-timer-interval 300
        twittering-number-of-tweets-on-retrieval 80
        twittering-icon-storage-file (concat forge-state-dir "twittering/icons.gz")
        twittering-user-id-db-file (concat forge-state-dir "twittering/user-id-info.gz")
        twittering-private-info-file (concat forge-state-dir "twittering/private.gpg")))


;;;
;;; Weather
;;; https://github.com/bcbcarl/emacs-wttrin
(use-package wttrin
  :ensure t
  :custom
  (wttrin-default-cities '("Eugene" "Portland" "Sonoma"))
  (wttrin-default-accept-language '("Accept-Language" . "en-US")))

;;;
;;;
;;;
(provide 'forge-util)
;;; forge-util.el ends here
