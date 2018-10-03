;;; forge-chat.el --- This is an Emacs Lisp file with Emacs Lisp code. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Stephen Fromm

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

(require 'notifications)
(require 'tls)

(defun forge/emoji-shrug () "Shrug emoji." (interactive) (insert "¯\\_(ツ)_/¯"))
(defun forge/emoji-glare () "Glare emoji." (interactive) (insert "ಠ_ಠ"))
(defun forge/emoji-table-flip () "Table fip emoji." (interactive) (insert "(╯°□°）╯︵ ┻━┻"))

;;;
;;; alerts
;;; https://github.com/jwiegley/alert
;;;
(use-package alert
    :ensure t
    :commands (alert)
    :init
    (setq alert-default-style 'notifier))


;;;
;;; Jabber
;;;
(defvar forge-jabber-account-alist nil
  "Alist of jabber account definitions.")

(use-package jabber
    :ensure t
    :preface
    (defun forge/jabber-start-or-switch ()
      "Connect to Jabber services"
      (interactive)
      (unless (get-buffer "*-jabber-roster-*")
        (jabber-connect-all))
      (if (or nil jabber-activity-jids)
          (jabber-activity-switch-to)
        (jabber-switch-to-roster-buffer)))
    :hook
    (jabber-post-connect . jabber-autoaway-start)
    :config
    (setq jabber-account-list forge-jabber-account-alist
          jabber-auto-reconnect t  ; reconnect automatically
          jabber-history-dir (concat forge-log-dir "jabber")
          jabber-history-enabled t ; enable logging
          jabber-history-muc-enabled t
          jabber-use-global-history nil
          jabber-backlog-number 40
          jabber-backlog-days 30
          jabber-chat-buffer-show-avatar t ; show avatar in chat buffer
          jabber-vcard-avatars-retrieve t ; automatically download vcard avatars
          jabber-alert-info-message-hooks (quote (jabber-info-echo jabber-info-display))
          jabber-alert-message-hooks (quote (jabber-message-notifications jabber-message-echo jabber-message-scroll))
          jabber-alert-presence-hooks (quote ()) ; don't show anything on presence changes
          jabber-alert-muc-hooks (quote (jabber-muc-notifications-personal jabber-muc-echo jabber-muc-scroll)))
                                        ; jabber uses the fsm package
    (setq fsm-debug nil)       ; defaults to "*fsm-debug*"
    (dolist (hook '(jabber-chat-mode-hook jabber-roster-mode-hook))
      (add-hook hook (lambda () "Disable yasnippet in jabber" (setq yas-dont-activate t)))))




;;;
;;; ERC and IRC
;;;
(defvar forge-erc-nick user-login-name
  "Default IRC login name.")

(defvar forge-erc-away-nick (concat forge-erc-nick "|afk")
  "Default IRC nick when away.")

(defvar forge-erc-keywords nil
  "Default keywords to look for.")

(defvar forge-erc-channels-alist nil
  "Default channels to log into.")

(use-package erc
    :defer t
    :preface
    (defun sf/erc-connect ()
      "Connect to IRC via ERC"
      (interactive)
      (when (y-or-n-p "Connect to freenode? ")
        (erc-tls :server "irc.freenode.net" :port 6697))
      (when (y-or-n-p "Connect to bitlbee? ")
        (progn
          (use-package bitlbee :demand t)
          (bitlbee-start)
          (sleep-for 2)
          (erc :server "localhost" :port 6667))))

    :config
    (use-package erc-match
        :after erc
        :config
        (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                        "324" "329" "333" "353" "477")))

    (setq erc-modules '(autojoin autoaway button completion fill irccontrols
                        list log match menu move-to-prompt netsplit
                        networks notifications readonly ring
                        services smiley spelling stamp track))
    (erc-services-mode t)
    (setq erc-nick forge-erc-nick
          erc-user-full-name forge-erc-nick
          erc-away-nickname forge-erc-away-nick
          erc-keywords forge-erc-keywords
          erc-auto-join-channels-alist forge-erc-channels-alist
          erc-insert-timestamp-function 'erc-insert-timestamp-left
          erc-timestamp-format "%H:%M:%S "
          ;; kill buffer after channel /part
          erc-kill-buffer-on-part t
          ;; kill buffer for server messages after /quit
          erc-kill-server-buffer-on-quit t
          ;; autoaway
          erc-auto-discard-away t
          erc-autoaway-use-emacs-idle t
          ;; logging
          erc-generate-log-file-name-function 'erc-generate-log-file-name-with-date
          erc-log-channels-directory (concat forge-log-dir "erc")
          erc-log-insert-log-on-open nil
          erc-prompt-for-nickserv-password nil
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


;;;
;;; Slack
;;; Follow the setup instructions for getting the client id, token, and so on.
;;; https://github.com/yuya373/emacs-slack#how-to-get-token
;;; http://endlessparentheses.com/keep-your-slack-distractions-under-control-with-emacs.html?source=rss
;;; TODO: call slack-register-team somewhere
;;;
(defvar forge-slack-client-id nil
  "Slack Client ID.")

(defvar forge-slack-client-token nil
  "Slack client token.")

(use-package slack
    :defer t
    :commands (slack-start)
    :bind (:map slack-mode-map
                ("C-c C-e" . slack-message-edit)
                ("C-c C-k" . slack-channel-leave)
                ("@" . slack-message-embed-mention)
                ("#" . slack-message-embed-channel))
    :init
    (setq slack-buffer-emojify t
          slack-prefer-current-team t))


(provide 'forge-chat)
;;; forge-chat.el ends here
