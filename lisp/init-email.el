;;; init-email.el --- Init email management -*- lexical-binding: t -*-
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


(require 'message)
(with-eval-after-load 'message
  (defun forge/mail-toggle-forward-mime ()
    "Toggle whether to forward as MIME or inline."
    (interactive)
    (if (bound-and-true-p message-forward-as-mime)
        (setq message-forward-as-mime nil)
      (setq message-forward-as-mime t)))
  (setq mail-from-style 'angles
        message-kill-buffer-on-exit t
        message-forward-as-mime t
        message-citation-line-format "On %a, %Y-%m-%d at %T %z, %N wrote:"
        message-citation-line-function (quote message-insert-formatted-citation-line)
        message-make-forward-subject-function (quote message-forward-subject-fwd)
        message-signature t
        message-signature-file "~/.signature"
        message-sendmail-envelope-from 'header
        message-send-mail-function 'message-send-mail-with-sendmail)
  (add-hook 'message-mode-hook #'footnote-mode)
  (add-hook 'message-mode-hook #'turn-on-flyspell)
  (add-hook 'message-mode-hook #'yas-minor-mode)
  (add-hook 'message-mode-hook #'turn-on-auto-fill
            (lambda ()
              (turn-on-auto-fill)
              (setq fill-column 72)
              (setq mail-header-separator ""))))

(require 'mm-decode)
(with-eval-after-load 'mm-decode
  (setq mm-text-html-renderer 'shr
        mm-inline-large-images nil
        mm-inline-text-html-with-images nil
        mm-discouraged-alternatives '("text/html" "text/richtext")))

(use-package gnus-alias
  :custom
  (gnus-alias-default-identity "work")
  :hook
  (message-setup . gnus-alias-determine-identity))

(add-hook 'dired-mode #'turn-on-gnus-dired-mode)
(with-eval-after-load
    (setq gnus-dired-mail-mode 'notmuch-user-agent))

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

(use-package boxquote)

(defun forge/mail-forward-complaint (template)
  "Forward an abuse complaint using TEMPLATE."
  (interactive)
  (if (boundp 'notmuch-mua-compose-in) (notmuch-show-forward-message) (mu4e-compose 'forward))
  (message-goto-body)
  (yas-expand-snippet (yas-lookup-snippet template))
  (message-add-header (concat "Cc: " forge-mail-abuse-poc))
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

(defun forge/mail-reply-to-abuse ()
  "Set Reply-To header to Abuse POC."
  (interactive)
  (message-add-header (concat "Reply-To: " forge-mail-abuse-poc)))

(defun forge/mail-reply-to-noc ()
  "Set Reply-To header to NOC POC."
  (interactive)
  (message-add-header (concat "Reply-To: " forge-mail-noc-poc)))

(defun forge/mail-org-notes ()
  "Send org notes as an email."
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-mime-org-subtree-htmlize)
    (message-goto-to)))

(defhydra forge/hydra-email (:color blue)
  "

  _A_ Forward Abuse report  _S_  Forward Spam report  _N_  Toggle compose New frame
  _I_ Forward Infringement  _C_  Comporomised report  _W_  Save all attachments
  _sd_ Search last day      _sw_ Search last week     _sm_ Search last month
  _ra_ Reply to Abuse POC   _rn_ Reply to NOC POC     _O_ Email Org notes
  "
  ("A" forge/mail-forward-abuse-complaint)
  ("ra" forge/mail-reply-to-abuse)
  ("rn" forge/mail-reply-to-noc)
  ("sd" notmuch-search-last-day)
  ("sw" notmuch-search-last-week)
  ("sm" notmuch-search-last-month)
  ("O" forge/mail-org-notes)
  ("I" forge/mail-forward-infringement-complaint)
  ("S" forge/mail-forward-spam-complaint)
  ("C" forge/mail-forward-compromised-complaint)
  ("W" forge/notmuch-save-all-attachments)
  ("N" forge/mail-toggle-compose-new-frame))

(global-set-key (kbd "C-c m") 'forge/hydra-email/body)

(with-eval-after-load 'sendmail
  (setq mail-specify-envelope-from t
        mail-envelope-from 'header
        sendmail-program (executable-find "sendmail.py")))

(use-package smtpmail
  :disabled t
  :config
  (setq smtpmail-stream-type 'ssl
        smtpmail-default-smtp-server forge-smtp-server-work
        smtpmail-smtp-server forge-smtp-server-work
        smtpmail-smtp-service 465
        smtpmail-smtp-user forge-smtp-user-work
        smtpmail-queue-dir (expand-file-name "queue" forge-state-dir)))

(with-eval-after-load 'mml-sec
  (setq mml-secure-openpgp-encrypt-to-self t
        mml-secure-openpgp-sign-with-sender t
        mml-secure-smime-encrypt-to-self t
        mml-secure-smime-sign-with-sender t)
  (add-to-list 'mml-secure-openpgp-signers "A852499F"))

(use-package notmuch
  :ensure nil
  :commands (notmuch)
  :custom
  (notmuch-search-oldest-first nil)
  (notmuch-hello-thousands-separator ",")
  (notmuch-crypto-process-mime t)
  (notmuch-show-part-button-default-action 'notmuch-show-view-part)

  :preface
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

  (defun notmuch-search-attachment (ext)
    "Search for attachments with extension EXT.

You can provide a space-delimited list of extensions to search for.
Will open a notmuch search buffer of the search results."
    (interactive "sExtension: ")
    (notmuch-search
     (mapconcat 'identity
                (mapcar (lambda (arg) (concat "attachment:" arg)) (split-string ext)) " or ")))

  (defun notmuch-search-recent (period &optional query)
    "Search for recent mail for time period PERIOD.

Prompts for  QUERY and this will amend the search to
limit it to the provided time PERIOD.
Will open a notmuch search buffer of the search results."
    (let* ((query (or query (notmuch-read-query "Query: "))))
      (notmuch-search (concat "date:" period ".. AND " query))))

  (defun notmuch-search-last-day (&optional query)
    "Search recent mail for prompted query.

Search notmuch for QUERY and this will amend the search to
limit it to the last day.
Will open a notmuch search buffer of the search results."
    (interactive)
    (notmuch-search-recent "1d" query))

  (defun notmuch-search-last-week (&optional query)
    "Search recent mail for prompted query.

Search notmuch for QUERY and this will amend the search to
limit it to the last 7 days.
Will open a notmuch search buffer of the search results."
    (interactive)
    (notmuch-search-recent "7d" query))

  (defun notmuch-search-last-month (&optional query)
    "Search recent mail for prompted query.

Search notmuch for QUERY and this will amend the search to
limit it to the last month.
Will open a notmuch search buffer of the search results."
    (interactive)
    (notmuch-search-recent "1M" query))

  :bind
  (:map notmuch-show-mode-map
        ("g" . notmuch-refresh-this-buffer)
        ("y" . notmuch-show-archive-message-then-next-or-next-thread)
        ("Y" . notmuch-show-archive-thread-then-next)
        ("d" . (lambda ()
                 "mark message for trash"
                 (interactive)
                 (forge-notmuch-show-tag (list "+trash" "-inbox" "-unread" "-archive"))))
        ("I" . (lambda ()
                 "mark message for inbox and delete trash, if present."
                 (interactive)
                 (forge-notmuch-show-tag (list "-trash" "+inbox"))))
        ("J" . (lambda ()
                 "mark message as junk"
                 (interactive)
                 (forge-notmuch-show-tag (list "+bulk" "+trash" "-inbox" "-unread" "-archive"))))
        ("F" . (lambda ()
                 "toggle message as flagged"
                 (interactive)
                 (forge-notmuch-show-toggle-tag "flagged")))
        ("M" . (lambda ()
                 "toggle message as muted"
                 (interactive)
                 (forge-notmuch-show-toggle-tag "mute")))
        ("b" . (lambda (&optional address)
                 "Bounce the current message"
                 (interactive "sBounce to: ")
                 (notmuch-show-view-raw-message)
                 (message-resend address)))
        ("S-SPC" . notmuch-show-rewind))
  (:map notmuch-search-mode-map
        ("g" . notmuch-refresh-this-buffer)
        ("y" . notmuch-search-archive-thread)
        ("Y" . notmuch-search-archive-thread)
        ("d" . (lambda ()
                 "mark thread for trash"
                 (interactive)
                 (forge-notmuch-search-tag (list "+trash" "-inbox" "-unread" "-archive"))))
        ("I" . (lambda ()
                 "mark message for inbox and delete trash tag, if present."
                 (interactive)
                 (forge-notmuch-search-tag (list "-trash" "+inbox"))))
        ("J" . (lambda ()
                 "mark thread as junk"
                 (interactive)
                 (forge-notmuch-search-tag (list "+bulk" "+trash" "-inbox" "-unread" "-archive"))))
        ("F" . (lambda ()
                 "toggle thread as flagged"
                 (interactive)
                 (forge-notmuch-search-toggle-tag "flagged")))
        ("M" . (lambda ()
                 "toggle thread as muted"
                 (interactive)
                 (forge-notmuch-search-toggle-tag "mute")))
        ("S-SPC" . notmuch-search-scroll-down))
  (:map notmuch-show-part-map
        ("c" . forge/notmuch-show-calendar-invite))

  :config
  (add-hook 'notmuch-show-hook '(lambda () (setq show-trailing-whitespace nil)))
  (setq notmuch-archive-tags '("-unread" "-inbox" "-trash" "-bulk" "-spam")
        notmuch-saved-searches '(( :name "📥 Inbox"
                                   :key "i"
                                   :query "tag:inbox")
                                 ( :name "🚩 Flagged"
                                   :key "f"
                                   :query "tag:flagged or tag:important")
                                 ( :name "📅 Today"
                                   :key "t"
                                   :query "date:24h.. and ( tag:inbox or tag:unread )")
                                 ( :name "💬 Unread"
                                   :key "u"
                                   :query "tag:unread")
                                 ( :name "Sent"
                                   :key "s"
                                   :query "tag:sent")
                                 ( :name "3 days"
                                   :key "3"
                                   :query "date:3d..  and ( tag:inbox or tag:unread )")
                                 ( :name "Last 7 days"
                                   :key "7"
                                   :query "date:7d..  and ( tag:inbox or tag:unread )")
                                 ( :name "Last 30 days"
                                   :key "m"
                                   :query "date:1M..1d and ( tag:inbox or tag:unread )")
                                 ( :name "Old messages"
                                   :key "o"
                                   :query "date:..1M and ( tag:inbox or tag:bulk or tag:unread ) ")
                                 ( :name "Attachments"
                                   :key "A"
                                   :query "tag:attachment")
                                 ( :name "Bulk"
                                   :key "B"
                                   :query "tag:unread and ( tag:bulk or tag:spam )")
                                 ( :name "Meeting Invites"
                                   :key "c"
                                   :query "mimetype:text/calendar"))))

(defun forge/twiddle-luminance (value)
  "Twiddle the luminance value to VALUE."
  (interactive "nLuminance: ")
  (message "Current luminance level: %s" shr-color-visible-luminance-min)
  (setq shr-color-visible-luminance-min value))

(defcustom forge-attachment-dir
  (expand-file-name "Downloads" "~/")
  "Directory to save attachments from email."
  :group 'forge
  :type 'string)

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

;; This is derived in part from notmuch-show-save-attachments
;; but calls mm-save-part-to-file instead so as to save files without prompting.
(defun forge/notmuch-save-all-attachments ()
  "Save all attachments in MSG to attachment directory."
  (interactive)
  (let* ((subject (message-wash-subject (notmuch-show-get-subject)))
         (attachdir (concat (file-name-as-directory forge-attachment-dir) subject)))
    (with-current-notmuch-show-message
     (let ((mm-handle (mm-dissect-buffer)))
       (message "%s" subject)
       (mkdir attachdir t)
       (notmuch-foreach-mime-part
        (lambda (p)
          (let ((disposition (mm-handle-disposition p)))
            (and (listp disposition)
                 (or (equal (car disposition) "attachment")
                     (and (equal (car disposition) "inline")
                          (assq 'filename disposition)))
                 (mm-save-part-to-file
                  p (concat (file-name-as-directory attachdir) (cdr (assq 'filename disposition)))))))
        mm-handle)))))

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

(define-prefix-command 'my-mail-search-map)
(define-key my-mail-search-map (kbd "s") 'notmuch-search)
(define-key my-mail-search-map (kbd "r") 'notmuch-search-last-week)
(define-key my-mail-search-map (kbd "m") 'notmuch-search-last-month)
(define-key my-mail-search-map (kbd "A") 'notmuch-search-attachment)

(define-prefix-command 'my-mail-map)
(define-key my-mail-map (kbd "N") 'forge/mail-toggle-compose-new-frame)
(define-key my-mail-map (kbd "W") 'forge/notmuch-save-all-attachments)
(define-key my-mail-map (kbd "ra") 'forge/mail-reply-to-abuse)
(define-key my-mail-map (kbd "rn") 'forge/mail-reply-to-noc)
(define-key my-mail-map (kbd "s") 'my-mail-search-map)
(global-set-key (kbd "C-c m") 'my-mail-map)

(provide 'init-email)
