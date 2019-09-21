;;; forge-mail.el -- Configure email.  -*- lexical-binding: t -*-

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


(require 'message)

(setq mail-from-style 'angles
      message-kill-buffer-on-exit t
      message-citation-line-format "On %a, %Y-%m-%d at %T %z, %N wrote:"
      message-citation-line-function (quote message-insert-formatted-citation-line)
      message-make-forward-subject-function (quote message-forward-subject-fwd)
      message-signature t
      message-signature-file "~/.signature"
      ;;
      message-sendmail-envelope-from 'header
      message-send-mail-function 'message-send-mail-with-sendmail
      ;;
      mime-view-text/html-previewer 'shr
      mm-text-html-renderer 'shr
      mm-inline-text-html-with-images nil
      mm-inline-large-images nil
      mm-discouraged-alternatives '("text/html" "text/richtext")
      ;;  mm-automatic-display (remove "text/html" mm-automatic-display))
      shr-inhibit-images nil)

(add-hook 'message-mode-hook 'footnote-mode)
(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'message-mode-hook #'yas-minor-mode)
(add-hook 'message-mode-hook 'turn-on-auto-fill
          (lambda()
            (auto-fill-mode t)
            (setq fill-column 72)
            (setq mail-header-separator "")))


;;;
;;; PGP
;;;
(defvar forge-openpgp-signers nil
  "A list of key ID(s) which will be used to sign a message.")

(setq mml-secure-openpgp-signers forge-openpgp-signers ; key to use for signing email
      mml-secure-openpgp-encrypt-to-self t)            ; also encrypt to self when sending email


;;;
;;; SMTP
;;;
(use-package smtpmail
    :defer t
    :disabled t
    :config
    (setq smtpmail-stream-type 'ssl
          smtpmail-default-smtp-server forge-smtp-server-work
          smtpmail-smtp-server forge-smtp-server-work
          smtpmail-smtp-service 465
          smtpmail-smtp-user forge-smtp-user-work
          smtpmail-queue-dir (expand-file-name (concat forge-state-dir "queue"))))

(use-package sendmail
    :defer t
    :custom
    (mail-specify-envelope-from t)
    (mail-envelope-from 'header)
    (sendmail-program (executable-find "smtpmail.py")))



;;;
;;; Variables for setting up email.
;;;
(defvar forge-maildir "~/.mail"
  "Path to Maildir.")

(defvar forge-attachment-dir "~/Download"
  "Path to where to save attachments to.")

(defvar forge-mail-abuse-cc nil
  "Default contact to CC on abuse reports.")

(defvar forge-fcc-dirs nil
  "Path to Fcc mail.")

(use-package notmuch
    :commands (notmuch)
    :bind
    (:map notmuch-search-mode-map
          ("y" . notmuch-search-archive-thread)
          ("S-SPC" . notmuch-search-scroll-down))
    (:map notmuch-show-mode-map
          ("y" . notmuch-show-archive-message-then-next-or-next-thread)
          ("S-SPC" . notmuch-show-rewind))
    (:map notmuch-show-part-map
          ("c" . forge/notmuch-show-calendar-invite))

    :init
    (add-hook 'notmuch-show-hook '(lambda () (setq show-trailing-whitespace nil)))
    (setq notmuch-archive-tags '("-unread" "-inbox" "-trash" "-bulk")
          notmuch-crypto-process-mime t
          notmuch-fcc-dirs forge-fcc-dirs
          notmuch-hello-thousands-separator ","
          notmuch-search-oldest-first nil
          notmuch-show-part-button-default-action 'notmuch-show-view-part
          notmuch-saved-searches '((:name "Inbox"           :key "i" :query "tag:inbox")
                                   (:name "Flagged"         :key "f" :query "tag:flagged or tag:important")
                                   (:name "Today"           :key "t" :query "date:24h.. and ( tag:inbox or tag:unread )")
                                   (:name "7 days"          :key "7" :query "date:7d..  and ( tag:inbox or tag:unread )")
                                   (:name "This week"       :key "y" :query "date:7d..1d and ( tag:inbox or tag:unread )")
                                   (:name "This month"      :key "m" :query "date:1M..1d and ( tag:inbox or tag:unread )")
                                   (:name "Old messages"    :key "o" :query "date:..1M and ( tag:inbox or tag:bulk or tag:unread ) ")
                                   (:name "Needs attention" :key "!" :query "tag:inbox and ( tag:abuse or tag:flagged )")
                                   (:name "Sent"            :key "s" :query "tag:sent")
                                   (:name "Attachments"     :key "A" :query "tag:attachment")
                                   (:name "Bulk"            :key "B" :query "tag:bulk and not tag:archive")
                                   (:name "Meeting Invites" :key "c" :query "mimetype:text/calendar")))

    :config
    (defmacro forge-notmuch-show-tag (tags)
      "Macro to take list of tags and apply to query."
      `(progn
         (notmuch-show-add-tag ,tags)
         (unless (notmuch-show-next-open-message)
           (notmuch-show-next-thread t))))

    (defmacro forge-notmuch-search-tag (tags)
      "Macro to take list of tags and apply to query."
      `(progn
         (notmuch-search-tag ,tags)
         (notmuch-search-next-thread)))

    (defmacro forge-notmuch-show-toggle-tag (tag)
      "Macro to toggle presence of tag for query."
      `(progn
         (if (member ,tag (notmuch-show-get-tags))
             (notmuch-show-remove-tag (list (concat "-" ,tag)))
           (notmuch-show-add-tag (list (concat "+" ,tag))))))

    (defmacro forge-notmuch-search-toggle-tag (tag)
      "Macro to toggle presence of tag for query."
      `(progn
         (if (member ,tag (notmuch-search-get-tags))
             (notmuch-search-tag (list (concat "-" ,tag)))
           (notmuch-search-tag (list (concat "+" ,tag))))))

    (define-key notmuch-show-mode-map (kbd "d")
      (lambda ()
        "mark message for trash"
        (interactive)
        (forge-notmuch-show-tag (list "+trash" "-inbox" "-unread" "-archive"))))

    (define-key notmuch-search-mode-map (kbd "d")
      (lambda ()
        "mark thread for trash"
        (interactive)
        (forge-notmuch-search-tag (list "+trash" "-inbox" "-unread" "-archive"))))

    (define-key notmuch-show-mode-map (kbd "D")
      (lambda ()
        "unmark message for trash and tag for inbox"
        (interactive)
        (forge-notmuch-show-tag (list "-trash" "+inbox"))))

    (define-key notmuch-search-mode-map (kbd "D")
      (lambda ()
        "unmark thread for trash and tag for inbox"
        (interactive)
        (forge-notmuch-search-tag (list "-trash" "+inbox"))))

    (define-key notmuch-show-mode-map (kbd "J")
      (lambda ()
        "mark message as junk"
        (interactive)
        (forge-notmuch-show-tag (list "+bulk" "+trash" "-inbox" "-unread" "-archive"))))

    (define-key notmuch-search-mode-map (kbd "J")
      (lambda ()
        "mark thread as junk"
        (interactive)
        (forge-notmuch-search-tag (list "+bulk" "+trash" "-inbox" "-unread" "-archive"))))

    (define-key notmuch-show-mode-map (kbd "F")
      (lambda ()
        "toggle message as flagged"
        (interactive)
        (forge-notmuch-show-toggle-tag "flagged")))

    (define-key notmuch-search-mode-map (kbd "F")
      (lambda ()
        "toggle thread as flagged"
        (interactive)
        (forge-notmuch-search-toggle-tag "flagged")))

    (define-key notmuch-show-mode-map (kbd "M")
      (lambda ()
        "toggle message as muted"
        (interactive)
        (forge-notmuch-show-toggle-tag "mute")))

    (define-key notmuch-search-mode-map (kbd "M")
      (lambda ()
        "toggle thread as muted"
        (interactive)
        (forge-notmuch-search-toggle-tag "mute")))

    (define-key notmuch-show-mode-map (kbd "b")
      (lambda (&optional address)
        "Bounce the current message"
        (interactive "sBounce to: ")
        (notmuch-show-view-raw-message)
        (message-resend address)))

    (define-key notmuch-search-mode-map (kbd "g") 'notmuch-refresh-this-buffer)
    (define-key notmuch-hello-mode-map  (kbd "g") 'notmuch-refresh-this-buffer))


;;;
;;; Helpers
;;;

;;;
;;; Pipe ICS part into the calendar.
;;;
(defun forge/mail-add-calendar-invite (handle &optional prompt)
  "Open calendar ICS part in Calendar."
  (ignore prompt)
  (mm-with-unibyte-buffer
    (mm-insert-part handle)
    (mm-add-meta-html-tag handle)
    (let ((path (expand-file-name "~/Download/invite.ics")))
      (mm-write-region (point-min) (point-max) path nil nil nil 'binary t)
      (start-process "add-calendar-invite" nil "/usr/bin/open" "-a" "/Applications/Microsoft Outlook.app" path))))

(defun forge/notmuch-show-calendar-invite ()
  "Save ics MIME part."
  (interactive)
  (notmuch-show-apply-to-current-part-handle #'forge/mail-add-calendar-invite))

;;;
;;; Pipe HTML part into a browser
;;;
(defun forge/mail-open-html ()
  "Open HTML part in browser."
  (interactive)
  (with-current-notmuch-show-message
      (let ((mm-handle (mm-dissect-buffer)))
        (notmuch-foreach-mime-part
         (lambda (p)
           (if (string-equal (mm-handle-media-type p) "text/html")
               (mm-display-part p "open")))
         ;;             (notmuch-show-view-part)))
         ;;             (notmuch-show-apply-to-current-part-handle #'mm-display-part)))
         mm-handle))))

;;;
;;; Misc helpers to forward abuse reports.
;;;
(defun forge/mail-forward-complaint (template)
  "Forward an abuse complaint using TEMPLATE."
  (interactive)
  (if (boundp 'notmuch-mua-compose-in) (notmuch-show-forward-message) (mu4e-compose 'forward))
  (message-goto-body)
  (yas-expand-snippet (yas-lookup-snippet template))
  (message-add-header (concat "Cc: " forge-mail-abuse-cc))
  (message-goto-to))

(defun forge/mail-forward-abuse-complaint ()
  "Forward an abuse complaint to responsible party."
  (interactive)
  (forge/mail-forward-complaint "abuse-template"))

(defun forge/mail-forward-infringement-complaint ()
  "Forward a infringement complaint to responsible party."
  (interactive)
  (forge/mail-forward-complaint "infringement-template"))

(defun forge/mail-forward-spam-complaint ()
  "Forward a spam complaint to responsible party."
  (interactive)
  (forge/mail-forward-complaint "spam-template"))

(defun forge/mail-forward-compromised-complaint ()
  "Forward a compromised account report to responsible party."
  (interactive)
  (forge/mail-forward-complaint "compromise-template"))

;;;
;;; Toggle whether to compose email in new frame.
;;;
(defun forge/mail-toggle-compose-new-frame ()
  "Toggle whether to compose email in new frame."
  (interactive)
  (let ((frame "same"))
    (if (boundp 'notmuch-mua-compose-in)
        (if (eq notmuch-mua-compose-in 'current-window)
            (progn (setq frame "new") (setq notmuch-mua-compose-in 'new-frame))
          (setq notmuch-mua-compose-in 'current-window))
      (if mu4e-compose-in-new-frame
          (setq mu4e-compose-in-new-frame nil)
        (progn (setq mu4e-compose-in-new-frame t) (setq frame "new"))))
    (message "Compose mail in %s frame" frame)))

(defhydra forge/hydra-email (:color blue)
  "

  _A_ Forward Abuse report  _S_ Forward Spam report  _N_ Toggle compose New frame
  _I_ Forward Infringement  _C_ Comporomised report
  "
  ("A" forge/mail-forward-abuse-complaint)
  ("I" forge/mail-forward-infringement-complaint)
  ("S" forge/mail-forward-spam-complaint)
  ("C" forge/mail-forward-compromised-complaint)
  ("N" forge/mail-toggle-compose-new-frame))


(global-set-key (kbd "C-c m") 'forge/hydra-email/body)


;;;
;;; Take attachment and submit to gcal-import.
;;;
;; (defun forge/mu4e-view-gcal-attachment (msg attachnum)
;;   "Feed MSG's attachment ATTACHNUM through gcal-import."
;;   (let* ((att (mu4e~view-get-attach msg attachnum))
;;          (pipecmd "gcal-import")
;;          (index (plist-get att :index)))
;;     (mu4e~view-temp-action (mu4e-message-field msg :docid) index "pipe" pipecmd)))

;;;
;;; Save all attachments in a message to path in Downloads folder that is named after the subject.
;;;
(defun forge/mu4e-save-all-attachments (&optional msg)
  "Save all attachments in MSG to attachment directory.
The sub-directory in `forge-attachment-dir' is derived from the subject of the email message."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)))
         (subject (message-wash-subject (mu4e-message-field msg :subject)))
         (attachdir (concat forge-attachment-dir "/" subject))
         (count (hash-table-count mu4e~view-attach-map)))
    (if (> count 0)
        (progn
          (mkdir attachdir t)
          (dolist (num (number-sequence 1 count))
            (let* ((att (mu4e~view-get-attach msg num))
                   (fname (plist-get att :name))
                   (index (plist-get att :index))
                   fpath)
              (setq fpath (expand-file-name (concat attachdir "/" fname)))
              (mu4e~proc-extract
               'save (mu4e-message-field msg :docid)
               index mu4e-decryption-policy fpath))))
      (message "Nothing to extract"))))

(defun forge/twiddle-luminance (value)
  "Twiddle the luminance value"
  (interactive "nLuminance: ")
  (message "Current luminance level: %s" shr-color-visible-luminance-min)
  (setq shr-color-visible-luminance-min value))


;;;
;;; org notmuch integration
;;;
(use-package org-notmuch
    :demand t
    :after (:any org notmuch))


;;;
;;; gnus-alias
;;; Mechanism to switch identities when using message mode
;;; https://www.emacswiki.org/emacs/GnusAlias
;;;
(defcustom forge-gnus-alias-identity nil
  "Alist with Mail account rules."
  :type 'list
  :group 'forge)

(defcustom forge-gnus-alias-identity-rules nil
  "Rules to determine which mail account to use."
  :type 'list
  :group 'forge)

(use-package gnus-alias
    :ensure t
    :custom
    (gnus-alias-default-identity "work")
    :hook (message-setup . gnus-alias-determine-identity)
    :init
    (setq gnus-alias-identity-alist forge-gnus-alias-identity
          gnus-alias-identity-rules forge-gnus-alias-identity-rules))


;;;
;;; Gnu Dired integration
;;; Attach files to email
;;; http://gnus.org/manual/gnus_129.html
;;;
(use-package gnus-dired
    :defer t
    :hook (dired-mode . turn-on-gnus-dired-mode)
    :init
    (setq gnus-dired-mail-mode 'notmuch-user-agent))

(provide 'forge-mail)
;;; forge-mail.el ends here
