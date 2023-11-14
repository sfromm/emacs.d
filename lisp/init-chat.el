;;; init-chat.el --- Init chat functionality -*- lexical-binding: t -*-
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


(with-eval-after-load 'erc
  (defun sf/erc-connect ()
    "Connect to IRC via ERC"
    (interactive)
    (when (y-or-n-p "Connect to libera? ")
      (erc-tls :server "irc.libera.chat" :port 6697))
    (when (y-or-n-p "Connect to bitlbee? ")
      (progn
        (use-package bitlbee :demand t)
        (bitlbee-start)
        (sleep-for 2)
        (erc :server "localhost" :port 6667))))

  (setq erc-nick user-login-name
        erc-server "irc.libera.chat"
        erc-away-nickname (concat erc-nick "|afk")
        erc-user-full-name erc-nick
        erc-fill-column 100
        erc-fill-static-center 16
        erc-fill-function 'erc-fill-static
        erc-modules '(autojoin autoaway button completion fill irccontrols
                               list log match menu move-to-prompt netsplit
                               networks notifications readonly ring
                               services smiley spelling stamp track))

  (erc-services-mode t)

  ;; use customize for `erc-keywords', and `erc-auto-join-channels-alist'
  (setq erc-prompt (lambda () (concat "[" (buffer-name) "]"))
        erc-insert-timestamp-function 'erc-insert-timestamp-left
        erc-timestamp-format "%H:%M:%S "
        erc-kill-buffer-on-part t         ;; kill buffer after channel /part
        erc-kill-server-buffer-on-quit t  ;; kill buffer for server messages after /quit
        erc-auto-discard-away t           ;; autoaway
        erc-autoaway-use-emacs-idle t
        ;; logging
        erc-generate-log-file-name-function 'erc-generate-log-file-name-with-date
        erc-log-channels-directory (expand-file-name "erc" forge-log-dir)
        erc-log-insert-log-on-open nil
        erc-prompt-for-nickserv-password nil
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "333" "353" "477")
        erc-save-buffer-on-part t))

(defun bitlbee-netrc-identify ()
  "Auto-identify for Bitlbee channels using authinfo or netrc.

   The entries that we look for in netrc or authinfo files have
   their 'port' set to 'bitlbee', their 'login' or 'user' set to
   the current nickname and 'server' set to the current IRC
   server's name.  A sample value that works for authenticating
   as user 'keramida' on server 'localhost' is:

   machine localhost port bitlbee login keramida password supersecret"
  (interactive)
  (when (string= (buffer-name) "&bitlbee")
    (let* ((secret (plist-get (nth 0 (auth-source-search :max 1
                                                         :host erc-server
                                                         :user (erc-current-nick)
                                                         :port "bitlbee"))
                              :secret))
           (password (if (functionp secret)
                         (funcall secret)
                       secret)))
      (erc-message "PRIVMSG" (concat (erc-default-target) " " "identify" " " password) nil))))
;; Enable the netrc authentication function for &biblbee channels.
(add-hook 'erc-join-hook 'bitlbee-netrc-identify)


(defvar forge-slack-client-id nil
  "Slack Client ID.")

(defvar forge-slack-client-token nil
  "Slack client token.")

(use-package slack
  :commands (slack-start)
  :bind (:map slack-mode-map
              ("C-c C-e" . slack-message-edit)
              ("C-c C-k" . slack-channel-leave)
              ("@" . slack-message-embed-mention)
              ("#" . slack-message-embed-channel))
  :init
  (setq slack-buffer-emojify t
        slack-prefer-current-team t))

(provide 'init-chat)
