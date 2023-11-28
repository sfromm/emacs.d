;;; init-utils.el --- Init various utilities -*- lexical-binding: t -*-
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


(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :init
  (setq nov-save-place-file (expand-file-name "nov-places" forge-state-dir)))

(use-package lorem-ipsum)

(with-eval-after-load 'go-jira
  (defvar jira-token nil)
  (defun jira-create ()
    "Create a ticket in Jira."
    (interactive)
    (unless jira-token
      (setq jira-token (lookup-password "go-jira.atlassian.net" user-login-name 6697)))
    (setenv "JIRA_API_TOKEN" jira-token)
    (require 'with-editor)
    (start-process "go-jira" (get-buffer-create " *go-jira*")
                   "jira" "create" "-b"
                   "--editor"
                   (concat with-editor-emacsclient-executable " -s " server-socket-dir "/server"))))


(use-package net-utils
  :commands (ping traceroute)
  :config
  (setq ping-program-options (list "-c" "5"))
  (setq traceroute-program-options (list "-I" "-m" "30" "-w" "1")))

(use-package ip-query
  :ensure nil
  :init (init-vc-install :fetcher "github" :repo "sfromm/ip-query")
  :commands (ip-query))


(use-package rg)

(use-package gist
  :custom (gist-view-gist t))


(use-package mastodon
  :custom
  (mastodon-client--token-file (expand-file-name "mastodon/mastodon.plstore" forge-state-dir))
  :config
  (mastodon-discover))

(use-package wttrin
  :ensure nil
  :init (init-vc-install :fetcher "github" :repo "sfromm/emacs-wttrin")
  :commands (wttrin)
  :custom
  (wttrin-default-cities '("Eugene" "Portland" "Sonoma" "Kapolei" "New Orleans"))
  (wttrin-language "en-US"))

(setq zoneinfo-style-world-list ; M-x shell RET timedatectl list-timezones or M-x dired RET /usr/share/zoneinfo
      '(("America/Los_Angeles" "Los Angeles")
        ("America/Denver" "Denver")
        ("America/Chicago" "Chicago")
        ("America/New_York" "New York")
        ("Canada/Atlantic" "Canada/Atlantic")
        ("UTC" "UTC")
        ("Europe/London" "London")
        ("Europe/Lisbon" "Lisbon")
        ("Europe/Brussels" "Barcelona • Paris • Brussels • Berlin")
        ("Europe/Athens" "Athens • Cairo • Kyiv")
        ("Asia/Tel_Aviv" "Tel Aviv")
        ("Asia/Kolkata" "Kolkata")
        ("Asia/Shanghai" "Beijing • Shanghai")
        ("Asia/Seoul" "Seoul")
        ("Asia/Tokyo" "Tokyo")
        ("Asia/Vladivostok" "Vladivostok")
        ("Australia/Brisbane" "Brisbane")
        ("Australia/Sydney" "Sydney")
        ("Pacific/Auckland" "Auckland")
        ("Pacific/Honolulu" "Honolulu")))
(setq world-clock-list t
      setq world-clock-buffer-name "*world-clock*"
      world-clock-time-format "%R %Z (%z)  %A %d %B")

(use-package with-editor)

(provide 'init-utils)
