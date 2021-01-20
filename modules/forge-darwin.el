;;; forge-darwin.el --- Set up helpers on darwin platform.  -*- lexical-binding: t -*-

;; Copyright (C) 2018-2021 Stephen Fromm

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

;;; Miscellaneous functions and support when on a Darwin platform.

;;; Code:

;;;
;;; Look up current playing song in itunes
;;; useful resources:
;;; - https://apple.stackexchange.com/questions/297240/getting-the-file-path-of-a-currently-playing-itunes-track-with-applescript
;;; - https://alvinalexander.com/blog/post/mac-os-x/applescript-concatenate-strings
;;;
(defun forge/get-current-song-itunes ()
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
    cursong))


;;;
;;; VPN helpers
;;;
(defvar forge-vpn-config ""
  "Name of the OpenVPN VPN configuration to use.")

(defun vpn-connect ()
  "Connect to VPN configuration CFG.
Assumes you are on MacOS and using Tunnelblick to connect."
  (interactive)
  (require 'em-glob)
  (let ((cfg (completing-read "Config: "
                              (mapcar #'file-name-sans-extension
                                      (directory-files "~/annex/etc" nil (eshell-glob-regexp "*ovpn"))))))
    (setq forge-vpn-config cfg)
    (when (forge/system-type-darwin-p)
      (let ((osatmpl ""))
        (setq osatmpl (concat "tell application \"/Applications/Tunnelblick.app\"\n"
                              "    connect \"" cfg "\"\n"
                              "end tell"))
        (do-applescript osatmpl)))))

(defun vpn-disconnect ()
  "Disconnect from VPN.
Assumes you are on MacOS and using Tunnelblick to manage your VPN."
  (interactive)
  (let ((osatmpl ""))
    (setq osatmpl (concat "tell application \"/Applications/Tunnelblick.app\"\n"
                          "    disconnect \"" forge-vpn-config "\"\n"
                          "end tell"))
    (do-applescript osatmpl)))


;;; WIP
(defun forge/apple_send_message ()
  "WIP Send message via Apple Messages."
  (interactive)
  (let ((osatmpl "")
        (message "")
        (recipient ""))
    (setq osatmpl (concat "tell application \"Messages\""
                          "    send"))))




(provide 'forge-darwin)
;;; forge-darwin.el ends here
