;;; init-elfeed.el --- Init Elfeed -*- lexical-binding: t -*-
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


(use-package elfeed
  :commands (elfeed)
  :bind
  (:map elfeed-search-mode-map
        ("j" . next-line)
        ("k" . previous-line)
        ("d" . elfeed-search-youtube-dl)
        ("f" . forge/elfeed-search-toggle-starred)
        ("o" . elfeed-search-mpv)
        ("J" . elfeed-unjam)
        ("S" . forge/elfeed-search-save-db)
        ("R" . forge/elfeed-search-mark-all-read)
        ("F" . forge/elfeed-search-starred)
        ("U" . forge/elfeed-search-unread)
        ("<" . forge/elfeed-search-first-article)
        (">" . forge/elfeed-search-last-article)
        :map elfeed-show-mode-map
        ("j" . elfeed-show-next)
        ("k" . elfeed-show-prev)
        ("d" . elfeed-show-youtube-dl)
        ("e" . elfeed-show-open-eww)
        ("f" . forge/elfeed-show-toggle-starred)
        ("o" . elfeed-show-mpv))
  :preface
  (defun forge/elfeed-load-db ()
    "Wrapper to load elfeed database from disk when running elfeed."
    (elfeed-db-load))

  (advice-add 'elfeed :before #'forge/elfeed-load-db)

  (defun forge/elfeed-stop-timer ()
    "Cancel elfeed-update-timer."
    (interactive)
    (when elfeed-update-timer (cancel-timer elfeed-update-timer)))

  (defun forge/elfeed-start-timer ()
    "Start elfeed-update-timer."
    (interactive)
    (setq elfeed-update-timer (run-at-time 180 (* 120 60) 'forge/elfeed-update)))

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

  (defun forge/elfeed-entry-url ()
    "Copy the current entry link URL to the clipboard."
    (interactive)
    (let ((entry))
      (if (eq major-mode 'elfeed-show-mode)
          (setq entry elfeed-show-entry)
        (setq entry (car (elfeed-search-selected))))
      (elfeed-entry-link entry)))

  (defun forge/elfeed-capture-entry-url ()
    "Set up for org capture to grab elfeed entry url."
    (interactive)
    (if (get-buffer "*elfeed-entry*")
        (with-current-buffer "*elfeed-entry*" (forge/elfeed-entry-url))
      (with-current-buffer "*elfeed-search*" (forge/elfeed-entry-url))))

  (defun forge/elfeed-entry-tags ()
    "Return entry tags as a string."
    (interactive)
    (let ((entry))
      (if (eq major-mode 'elfeed-show-mode)
          (setq entry elfeed-show-entry)
        (setq entry (car (elfeed-search-selected))))
      (upcase (mapconcat #'symbol-name (elfeed-entry-tags entry) ":"))))

  (defun forge/elfeed-get-entry-tags ()
    "Set up for org capture to grab elfeed entry tags."
    (interactive)
    (if (get-buffer "*elfeed-entry*")
        (with-current-buffer "*elfeed-entry*" (forge/elfeed-entry-tags))
      (with-current-buffer "*elfeed-search*" (forge/elfeed-entry-tags))))

  (defun elfeed-show-open-eww ()
    "Open the current entry with eww."
    (interactive)
    (eww (elfeed-entry-link elfeed-show-entry))
    (add-hook 'eww-after-render-hook 'eww-readable nil t))

  (defun forge/elfeed-search-starred ()
    "Show starred elfeed articles"
    (interactive)
    (elfeed-search-set-filter "+starred"))

  (defun forge/elfeed-search-unread ()
    "Show elfeed articles tagged with unread"
    (interactive)
    (elfeed-search-set-filter "@6-months-ago +unread"))

  (defun forge/elfeed-search-save-db ()
    "Save elfeed database to disk."
    (interactive)
    (elfeed-db-save)
    (message "elfeed db saved."))

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

  ;; Lastly, the following will helps with downloading videos from Youtube when they
  ;; are part of a RSS feed.
  (with-eval-after-load 'youtube-dl
    (setq youtube-dl-directory "~/media/youtube"
          ;; youtube-dl-arguments '("--no-mtime" "--restrict-filenames" "--ignore-config")
          youtube-dl-program "yt-dlp"))

  (elfeed-org)
  (setq url-queue-timeout 30
        elfeed-db-directory (expand-file-name "elfeed" (concat (getenv "HOME") "/annex/var"))))
        ;; create timer to update elfeed
        ;; elfeed-update-timer (run-at-time 180 (* 120 60) 'forge/elfeed-update)))

(use-package elfeed-org
  :after (:all org elfeed)
  :commands (elfeed-org)
  :config
  (setq rmh-elfeed-org-files (list (expand-file-name "elfeed.org" org-directory)))
  (elfeed-org))

(provide 'init-elfeed)
