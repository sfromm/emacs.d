;;; forge-elfeed.el --- Configure elfeed RSS reader.  -*- lexical-binding: t -*-

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

(use-package youtube-dl
    :init (setq youtube-dl-directory "~/annex/Video/youtube"))

(use-package elfeed
  :ensure elfeed
  :bind (:map elfeed-search-mode-map
              ("j" . next-line)
              ("k" . previous-line)
              ("d" . elfeed-search-youtube-dl)
              ("f" . forge/elfeed-search-toggle-starred)
              ("o" . elfeed-search-mpv)
              ("J" . elfeed-unjam)
              ("R" . forge/elfeed-search-mark-all-read)
              ("F" . forge/elfeed-search-starred)
              ("U" . forge/elfeed-search-unread)
              ("E" . forge/elfeed-search-emacs)
              ("N" . forge/elfeed-search-news)
              ("T" . forge/elfeed-search-tech)
              ("<" . forge/elfeed-search-first-article)
              (">" . forge/elfeed-search-last-article)
              :map elfeed-show-mode-map
              ("j" . elfeed-show-next)
              ("k" . elfeed-show-prev)
              ("d" . elfeed-show-youtube-dl)
              ("e" . elfeed-show-open-eww)
              ("f" . forge/elfeed-show-toggle-starred)
              ("o" . elfeed-show-mpv))
  :config
  (defun elfeed-search-mpv ()
    "Play the current entry with mpv"
    (interactive)
    (message "url %s" (elfeed-entry-link (car (elfeed-search-selected))))
    (start-process "*elfeed-mpv*" nil "mpv" (elfeed-entry-link (car (elfeed-search-selected)))))

  (defun elfeed-show-mpv ()
    "Play the current entry with mpv"
    (interactive)
    (start-process "*elfeed-mpv*" nil "mpv" (elfeed-entry-link elfeed-show-entry)))

  (defun elfeed--youtube-dl (entry)
    "Download a video for ENTRY via youtube-dl."
    (if (null (youtube-dl (elfeed-entry-link entry)
                          :title (elfeed-entry-title entry)
                          ;; elfeed-feed-author will return a list of plist that will look like ((:name "HappyBlog" :uri "https://example.com/happyblog"))
                          :directory (concat youtube-dl-directory "/" (plist-get (car (elfeed-feed-author (elfeed-entry-feed entry))) :name))))
        (message "Entry is not a youtube link")
      (message "Downloading %s" (elfeed-entry-title entry))))

  ;; from skeeto
  ;; https://github.com/skeeto/.emacs.d/blob/master/etc/feed-setup.el
  (defun elfeed-search-youtube-dl ()
    "Download the current entry/entries with youtube-dl"
    (interactive)
    (let ((entries (elfeed-search-selected)))
      (dolist (entry entries)
        (elfeed--youtube-dl entry)
        (elfeed-untag entry 'unread)
        (elfeed-search-update-entry entry)
        (unless (use-region-p) (forward-line)))))

  ;; from skeeto
  ;; https://github.com/skeeto/.emacs.d/blob/master/etc/feed-setup.el
  (defun elfeed-show-youtube-dl ()
    "Download the current entry with youtube-dl"
    (interactive)
    (elfeed--youtube-dl elfeed-show-entry))

  (defun elfeed-show-open-eww ()
    "Open the current entry with eww."
    (interactive)
    (eww (elfeed-entry-link elfeed-show-entry))
    (add-hook 'eww-after-render-hook 'eww-readable nil t))

  (defun forge/elfeed-search-starred ()
    "Show starred elfeed articles"
    (interactive)
    (elfeed-search-set-filter "+starred"))

  (defun forge/elfeed-search-emacs ()
    "Show elfeed articles tagged with emacs"
    (interactive)
    (elfeed-search-set-filter "@6-months-ago +emacs +unread"))

  (defun forge/elfeed-search-tech ()
    "Show elfeed articles tagged with tech"
    (interactive)
    (elfeed-search-set-filter "@6-months-ago +tech +unread"))

  (defun forge/elfeed-search-news ()
    "Show elfeed articles tagged with news"
    (interactive)
    (elfeed-search-set-filter "@6-months-ago +news +unread"))

  (defun forge/elfeed-search-unread ()
    "Show elfeed articles tagged with unread"
    (interactive)
    (elfeed-search-set-filter "@6-months-ago +unread"))

  ;; from manuel uberti
  ;; https://manuel-uberti.github.io/emacs/2017/08/01/elfeed/
  (defun forge/elfeed-search-mark-all-read ()
    "Mark all articles as read."
    (interactive)
    (call-interactively 'mark-whole-buffer)
    (elfeed-search-untag-all-unread))

  (defalias 'forge/elfeed-search-toggle-starred (elfeed-expose #'elfeed-search-toggle-all 'starred))

  (defun forge/elfeed-show-toggle-starred ()
    "Toggle starred tag for elfeed article"
    (interactive)
    (forge/elfeed-show-toggle-tag 'starred))

  (defun forge/elfeed-show-toggle-tag (tag)
    "Toggle tag for elfeed article."
    (interactive)
    (if (elfeed-tagged-p tag elfeed-show-entry)
        (elfeed-show-untag tag)
      (elfeed-show-tag tag)))

  (defun forge/elfeed-update ()
    "Update elfeed database."
    (message "Updating elfeed articles...")
    (elfeed-update)
    (elfeed-db-save))

  (defun forge/elfeed-search-first-article ()
    "Go to first message in search."
    (interactive)
    (goto-char (point-min)))

  (defun forge/elfeed-search-last-article ()
    "Go to last message in search."
    (interactive)
    (goto-char (point-max)))

  (defface elfeed-search-starred-title-face '((t :foreground "#f77"))
    "Marks a starred Elfeed entry.")
  (push '(starred elfeed-search-starred-title-face) elfeed-search-face-alist)

  (setq url-queue-timeout 30
        elfeed-db-directory (concat forge-state-dir "elfeed")
        ;; create timer to update elfeed
        elfeed-update-timer (run-at-time 180 (* 120 60) 'forge/elfeed-update)))


;;; elfeed-org
;;; https://github.com/remyhonig/elfeed-org
;;; Configure elfeed RSS feeds using org-mode.
(use-package elfeed-org
    :ensure t
    :after (:all org elfeed)
    :config
    (setq rmh-elfeed-org-files (list (concat org-directory "/elfeed.org")))
    (elfeed-org))

(provide 'forge-elfeed)
;;; forge-elfeed.el ends here
