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


(use-package ham
  :ensure nil
  :init (init-vc-install :fetcher "github" :repo "sfromm/ham-el")
  :commands (fle-mode pota)
  :magic ("mycall " . fle-mode)
  :config
  (setq-local tab-width 8))


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

(defun my-eshell-sntp-set-time (&optional server)
  "Set time from sntp server SERVER."
  (interactive "P")
  (let ((server (or server "time.apple.com")))
    (eshell-command (concat "sudo sntp -sS " server))))


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

(defmacro forge-mkhome-target (target)
  "Macro to run mkhome makefile TARGET."
  `(with-temp-buffer
     (progn
       (cd (getenv "HOME"))
       (compile (mapconcat 'shell-quote-argument (list "make" "-f" "Makefile.mkhome" ,target) " ")))))

(defun forge-mkhome-update ()
  "Run mkhome git."
  (interactive)
  (forge-mkhome-target "update"))

(defun forge-mkhome-www ()
  "Run mkhome www."
  (interactive)
  (forge-mkhome-target "www"))

(defun forge-mkhome-src ()
  "Run mkhome src."
  (interactive)
  (forge-mkhome-target "src"))


(when (forge/system-type-darwin-p)
  (defun my-get-current-song-itunes ()
    "Get current song playing via itunes."
    (let ((osa-tmpl "")
          (cursong nil))
      (setq osa-tmpl "tell application \"Music\"
	if player state is not stopped then
		set ct to (properties of current track)
		set this_song to \"\"
		if (class of ct is URL track) and (get current stream title) is not missing value then
			set this_song to (get current stream title)
		else
			set this_song to artist in ct & \" - \" & name in ct
		end if
		this_song
	end if
end tell")
      (condition-case nil
          (setq cursong (split-string (do-applescript osa-tmpl) " - "))
        (error nil))
      cursong)))

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


;; What follows are various helper functions that are used
;; either interactively or in other parts of the configuration.

(defun my-reload-emacs-configuration ()
  "Reload emacs configuration."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(defun forge/turn-off-delete-trailing-whitespace ()
  "Turn off `delete-trailing-whitespace' when saving files."
  (remove-hook 'before-save-hook 'delete-trailing-whitespace t))

;; Via jwiegley
;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
(defun lookup-password (host user port)
  "Look up password for HOST, USER, and PORT."
  (require 'auth-source)
  (require 'auth-source-pass)
  (let ((auth (auth-source-search :host host :user user :port port)))
    (if auth
        (let ((secretf (plist-get (car auth) :secret)))
          (if secretf
              (funcall secretf)
            (error "Auth entry for %s@%s:%s has no secret!"
                   user host port)))
      (error "No auth entry found for %s@%s:%s" user host port))))

;; Via https://emacs.stackexchange.com/questions/8104/is-there-a-mode-to-automatically-update-copyright-years-in-files
(defun forge/enable-copyright-update ()
  "Update copyright year when saving a file."
  (when (fboundp 'copyright-update)
    (setq copyright-names-regexp "Free Software")
    (add-hook 'before-save-hook #'copyright-update)))

;; Delete window if not the only one.
(defun forge/delete-window ()
  "Delete window if it is not the only one."
  (when (not (one-window-p))
    (delete-window)))

(defun forge/transparency (value)
  "Set the transparency of the frame window with VALUE 0=transparent/100=opaque."
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

(defun forge/untabify-buffer ()
  "Remove tab characters from buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun sudo-file-path (file)
  "Return path for FILE with sudo access."
  (let ((host (or (file-remote-p file 'host) "localhost")))
    (concat "/" (when (file-remote-p file)
                  (concat (file-remote-p file 'method) ":"
                          (if-let (user (file-remote-p file 'user))
                              (concat user "@" host) host)
                          "|"))
            "sudo:root@" host
            ":" (or (file-remote-p file 'localname)
                    file))))

(defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (find-file (sudo-file-path file)))

(defun sudo-this-file ()
  "Open current file as root."
  (interactive)
  (find-file
   (sudo-file-path
    (or buffer-file-name
        (or buffer-file-name
            (when (or (derived-mode-p 'dired-mode)
                      (derived-mode-p 'wdired-mode))
              default-directory))))))

(defconst speed_of_light 299792458 "Speed of light, m/s.")

(defun wavelength-to-frequency (wavelength)
  "Convert a wavelength to frequency."
  (interactive "nWavelength: ")
  (message "Frequency: %0.2f" (/ (/ speed_of_light wavelength) 1000)))

(defun frequency-to-wavelength (frequency)
  "Convert a frequency to wavelength (nm)."
  (interactive "nFrequency: ")
  (message "Wavelength: %0.4f" (/ (/ speed_of_light frequency) 1000)))

(defun forge/date-today ()
  "Insert friendly date string for today."
  (interactive)
  (insert (format-time-string "%B %d, %Y" (current-time))))

(use-package with-editor)

(provide 'init-utils)
