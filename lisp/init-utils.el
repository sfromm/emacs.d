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


(use-package net-utils
  :commands (ping traceroute)
  :config
  (setq ping-program-options (list "-c" "5"))
  (setq traceroute-program-options (list "-I" "-m" "30" "-w" "1")))

(use-package ip-query
  :ensure nil
  :init (init-vc-install :fetcher "github" :repo "sfromm/ip-query")
  :commands (ip-query ip-query-asn))

(defun dig-extended (fn &optional
                        domain query-type query-class query-option dig-option server)
  "Wrapper for `dig'.
Query for DNS records for DOMAIN of QUERY-TYPE."
  (message "domain: '%s'" domain)
  (unless domain
    (setq domain (read-string "Host: ")))
  (unless query-type
    (setq query-type (completing-read "Type: " '("A" "SOA" "NS" "TXT" "CNAME" "PTR"))))
  (funcall fn domain query-type query-class query-option dig-option server))

(advice-add 'dig :around #'dig-extended)


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
      world-clock-buffer-name "*world-clock*"
      world-clock-time-format "%R %Z (%z)  %A %d %B")


(defvar forge/vpn-config ""
  "Name of the OpenVPN VPN configuration to use.")

(when (forge/system-type-darwin-p)
  (defun vpn-connect ()
    "Connect to VPN configuration CFG.
Assumes you are are on MacOS and using Wireguard to connect."
    (interactive)
    (require 'em-glob)
    (let ((cfg (completing-read "Config: "
                                (mapcar #'file-name-sans-extension
                                        (directory-files "~/annex/etc" nil (eshell-glob-regexp "wg*conf"))))))
      (setq forge/vpn-config cfg)
      (when (forge/system-type-darwin-p)
        (shell-command (concat "scutil --nc start " cfg)))))

  (defun vpn-disconnect ()
    "Disconnect VPN configuration CFG.
Assumes you are are on MacOS and using Wireguard to connect."
    (interactive)
    (when (forge/system-type-darwin-p)
      (shell-command (concat "scutil --nc stop " forge/vpn-config))))

  (defun openvpn-connect ()
    "Connect to OpenVPN configuration CFG.
Assumes you are on MacOS and using Tunnelblick to connect."
    (interactive)
    (require 'em-glob)
    (let ((cfg (completing-read "Config: "
                                (mapcar #'file-name-sans-extension
                                        (directory-files "~/annex/etc" nil (eshell-glob-regexp "*ovpn"))))))
      (setq forge/vpn-config cfg)
      (when (forge/system-type-darwin-p)
        (let ((osatmpl ""))
          (setq osatmpl (concat "tell application \"/Applications/Tunnelblick.app\"\n"
                                "    connect \"" cfg "\"\n"
                                "end tell"))
          (do-applescript osatmpl)))))

  (defun openvpn-disconnect ()
    "Disconnect from VPN.
Assumes you are on MacOS and using Tunnelblick to manage your VPN."
    (interactive)
    (let ((osatmpl ""))
      (setq osatmpl (concat "tell application \"/Applications/Tunnelblick.app\"\n"
                            "    disconnect \"" forge/vpn-config "\"\n"
                            "end tell"))
      (do-applescript osatmpl))))

(defun my-org-web-bookmarks (path)
  "Return all HTTP links from an org-file at PATH."
  (with-temp-buffer
    (let (links)
      (insert-file-contents path)
      (org-mode)
      (org-element-map (org-element-parse-buffer) 'link
        (lambda (link)
          (let* ((raw-link (org-element-property :raw-link link))
                 (content (org-element-contents link))
                 (title (substring-no-properties (or (seq-first content) raw-link))))
            (when (string-prefix-p "http" raw-link)
              (push (concat title "\n" (propertize raw-link 'face 'whitespace-space) "\n")
                    links))))
        nil nil 'link)
      (seq-sort 'string-greaterp links))))

(defun my-open-browser-bookmark ()
  "Send a bookmark to the browser from the bookmark file."
  (interactive)
  (browse-url
   (seq-elt
    (split-string
     (completing-read "Open: " (my-org-web-bookmarks (expand-file-name "notebook.org" org-directory))) "\n") 1)))

(use-package with-editor)

(provide 'init-utils)
