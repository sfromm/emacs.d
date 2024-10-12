;;; init-org.el --- Init orgmode -*- lexical-binding: t -*-
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


(use-package org
  :preface
  (defun my-org-mode-hook ()
    "Turn on settings for org-mode."
    (interactive)
    (when (fboundp 'turn-off-auto-fill)
      (turn-off-auto-fill))
    (when (fboundp 'turn-on-flyspell)
      (turn-on-flyspell)))

  (defun init-my-org-agenda ()
    "Initialze org-agenda configuration."
    (interactive)
    (setq org-agenda-skip-scheduled-if-deadline-is-shown t
          org-agenda-sticky t
          org-agenda-hide-tags-regexp "."
          org-agenda-restore-windows-after-quit t
          org-agenda-window-setup 'current-window
          org-agenda-compact-blocks nil
          org-agenda-files
          (list (concat org-directory "/inbox.org")
                (concat org-directory "/agenda.org")
                (concat org-directory "/journal.org")
                (concat org-directory "/work.org")
                (concat org-directory "/personal.org"))
          org-agenda-prefix-format
          '((agenda . " %i %-12:c%?-12t% s")
            (todo   . " %i %-12:c")
            (tags   . " %i %-12:c")
            (search . " %i %-12:c")))
    ;; There's a lot to org-agenda-custom-commands
    ;; For type:
    ;;   type     The command type, any of the following symbols:
    ;;     agenda      The daily/weekly agenda.
    ;;     todo        Entries with a specific TODO keyword, in all agenda files.
    ;;     search      Entries containing search words entry or headline.
    ;;     tags        Tags/Property/TODO match in all agenda files.
    ;;     tags-todo   Tags/P/T match in all agenda files, TODO entries only.
    ;;     todo-tree   Sparse tree of specific TODO keyword in *current* file.
    ;;     tags-tree   Sparse tree with all tags matches in *current* file.
    ;;     occur-tree  Occur sparse tree for *current* file.
    (setq org-agenda-custom-commands
          '(("2" "Next two weeks"
             ((agenda ""
                      ((org-agenda-start-on-weekday nil)
                       ;; (org-agenda-start-day "+1d")
                       (org-agenda-span 14)
                       (org-deadline-warning-days 0)
                       (org-agenda-block-separator nil)
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                       (org-agenda-overriding-header "\n📅 Next 14 days\n")))))
            ("g" "Overview"
             ((agenda ""
                      ((org-agenda-overriding-header "🕐 Today\n")
                       (org-agenda-span 1)
                       (org-deadline-warning-days 0)
                       (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                       (org-agenda-block-separator nil)))
              (agenda ""
                      ((org-agenda-start-on-weekday nil)
                       (org-agenda-start-day "+1d")
                       (org-agenda-span 'week)
                       (org-deadline-warning-days 0)
                       (org-agenda-block-separator nil)
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                       (org-agenda-overriding-header "\n📅 Next 7 days\n")))
              (tags-todo "inbox"
                         ((org-agenda-prefix-format "  %?-12t% s")
                          (org-agenda-block-separator nil)
                          (org-agenda-overriding-header "\n📥 Inbox\n")))
              (agenda ""
                      ((org-agenda-time-grid nil)
                       (org-agenda-start-on-weekday nil)
                       ;; We don't want to replicate the previous section's
                       ;; three days, so we start counting from the day after.
                       (org-agenda-start-day "+3d")
                       (org-agenda-span 14)
                       (org-agenda-show-all-dates nil)
                       (org-deadline-warning-days 0)
                       (org-agenda-block-separator nil)
                       (org-agenda-entry-types '(:deadline))
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                       (org-agenda-overriding-header "\n🞜 Upcoming deadlines (+14d)\n")))
              (todo "NEXT"
                    ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                     (org-agenda-prefix-format "  %i %-12:c [%e] ")
                     (org-agenda-block-separator nil)
                     (org-agenda-overriding-header "\nNext\n")))
              (todo "WAITING"
                    ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                     (org-agenda-prefix-format "  %i %-12:c [%e] ")
                     (org-agenda-block-separator nil)
                     (org-agenda-overriding-header "\n💤 Waiting\n")))
              (todo "PROJECT"
                    ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                     (org-agenda-prefix-format "  %i %-12:c [%e] ")
                     (org-agenda-block-separator nil)
                     (org-agenda-overriding-header "\n🚧 Projects\n")))
              (tags-todo "SOMEDAY"
                         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                          (org-agenda-prefix-format "  %i %-12:c [%e] ")
                          (org-agenda-block-separator nil)
                          (org-agenda-overriding-header "\n💤 Someday\n")))
              (tags "CLOSED>=\"<-10d>\""
                    ((org-agenda-overriding-header "\n✓ Completed last 10 days\n"))))))))

  (defun init-my-org-capture-templates ()
    "Set up org-capture-templates."
    (interactive)
    ;; For template expansion,
    ;; see https://orgmode.org/manual/Template-expansion.html#Template-expansion
    (setq org-capture-templates
          `(("i" "Inbox" entry
             (file "inbox.org")
             "* TODO %? \n:PROPERTIES:\n:CAPTURED:  %U\n:END:\nReference: %a\n")
            ("c" "Calendar invite" entry
             (file "inbox.org")
             (function notmuch-calendar-capture-event)
             :prepend t)
            ("l" "Log" entry
             (file+olp+datetree "journal.org")
             "* %U - %?\n")
            ("n" "Meeting notes" entry
             (file+olp+datetree "journal.org")
             "* Notes - %a \n:PROPERTIES:\n:CAPTURED:  %U\n:END:\n%U\nAttendees:\n\nAgenda:\n\nDiscussion:\n"
             :clock-in t
             :clock-resume t)
            ("j" "Journal" entry
             (file+olp+datetree "journal.org")
             "* %?\n%U\n"
             :clock-in t
             :clock-resume t)
            ("b" "Bookmark" entry
             (file+headline "notebook.org" "Unfiled")
             "* %^L %^g \n:PROPERTIES:\n:CAPTURED: %U\n:END:\n\n"
             :prepend t)

            ("r" "Reference")
            ("rm" "Music" entry
             (file+olp+datetree "journal.org")
             "* %(forge/capture-current-song) :music:\n%U\n")
            ("rr" "Reference" entry
             (file+olp+datetree "articles.org")
             "* %a %?\n:PROPERTIES:\n:CAPTURED:  %U\n:END:\n"
             :prepend t)
            ("rw" "Web Page" entry
             (file+olp+datetree "articles.org")
             (function my-org-clip-web-page)
             :prepend t)
            ("rf" "Elfeed/News Article" entry
             (file+olp+datetree "articles.org")
             "* %a %? :%(forge/elfeed-get-entry-tags):ARTICLE:\n:PROPERTIES:\n:CAPTURED:  %U\n:END:\n"
             :prepend t)
            ("rt" "Twitter Post" entry
             (file+olp+datetree "articles.org")
             "* %a %? :TWITTER:\n:PROPERTIES:\n:CAPTURED:  %U\n:END:\n"
             :prepend t))))

  (defun my-org-goto-journal-entry (date)
    "Go to specified journal entry for DATE and narrow to subtree."
    (interactive (list (org-read-date)))
    (let* ((date-list (org-date-to-gregorian date))
           (year (number-to-string (nth 2 date-list))))
      (find-file (expand-file-name "journal.org" org-directory))
      (widen)
      (org-datetree-find-date-create date-list)))

  (defun my-org-goto-relative-journal-entry (offset)
    "Go to a relative date's journal entry in the datetree and narrow to subtree."
    (interactive "Enter date offset (e.g., +3, fri, +2tue): ")
    (let ((date (org-read-date nil nil offset)))
      (my-org-goto-journal-entry date)))

  (defun my-org-goto-yesterday-journal-entry ()
    "Go to yesterday's journal entry in the datetree and narrow to subtree."
    (interactive)
    (my-org-goto-relative-journal-entry "-1"))

  (defun my-org-goto-tomorrow-journal-entry ()
    "Go to tomorrow's journal entry in the datetree and narrow to subtree."
    (interactive)
    (my-org-goto-relative-journal-entry "+1"))

  (defun my-org-init-hook ()
    "Set up defaults after org.el has been loaded."
    (init-my-org-agenda)
    (init-my-org-capture-templates))

  (defun my-org-fixed-font-faces ()
    "Keep the following with fixed-pitch fonts."
    (interactive)
    (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-block nil :inherit 'fixed-pitch))

  (defun my-tangle-org-mode-on-save ()
    "Tangle org-mode file when saving -- but not org archive files."
    (when (string= (message "%s" major-mode) "org-mode")
      (unless (string-suffix-p ".org_archive" buffer-file-name)
        (org-babel-tangle))))

  (defun my-org-set-property (property value)
    "Set arbitrary PROPERTY to VALUE for current heading."
    (org-back-to-heading)
    (when (not (org-element-property :CREATED (org-element-at-point)))
      (org-set-property property value)))

  (defun my-org-set-uuid ()
    "Set ID property for current headline."
    (interactive)
    (my-org-set-property "ID" (org-id-uuid)))

  (defun my-org-set-created ()
    "Set CREATED property for current headline."
    (interactive)
    (my-org-set-property "CREATED" (with-temp-buffer (org-insert-time-stamp (current-time) t t))))

  (defun my-org-timer-clock-in ()
    "Clock in when starting a org-timer."
    (if (eq major-mode 'org-agenda-mode)
        (call-interactively 'org-agenda-clock-in)
      (call-interactively 'org-clock-in)))

  (defun my-org-timer-clock-out ()
    "Clock in when starting a org-timer."
    (if (eq major-mode 'org-agenda-mode)
        (call-interactively 'org-agenda-clock-out)
      (call-interactively 'org-clock-out)))

  (defun my-org-set-properties ()
    "Set stock org properties for current headline."
    (interactive)
    (my-org-set-uuid)
    (my-org-set-created))

  (defun my-org-clip-web-page ()
    "Clip web page for org capture."
    (interactive)
    (when (derived-mode-p 'eww-mode)
      (require 'ol-eww)
      (org-eww-copy-for-org-mode)
      (concat
       "* %a %? :ARTICLE:
  :PROPERTIES:
  :CREATED:  %U
  :URL:      " (eww-current-url) "
  :END:\n\n" (car kill-ring))))


  :hook
  ((org-mode . my-org-mode-hook)
   (after-init . my-org-init-hook)
   (after-save . my-tangle-org-mode-on-save)
   (org-timer-set . my-org-timer-clock-in)
   (org-timer-done . my-org-timer-clock-out)
   (org-timer-stop . my-org-timer-clock-out)
   (org-mode . variable-pitch-mode))

  :custom
  (org-attach-id-dir "~/annex/org/data/")
  (org-directory "~/forge")
  (org-attach-method 'mv)
  (org-babel-python-command "python3")
  (org-catch-invisible-edits 'smart)
  (org-clock-display-default-range 'thisweek)
  (org-clock-in-resume t)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-persist t)
  (org-clock-sound (expand-file-name "drip.ogg" "~/annex/Music/"))
  (org-confirm-babel-evaluate nil)
  (org-default-notes-file (expand-file-name "journal.org" org-directory))
  (org-ellipsis "⤵")
  (org-export-allow-bind-keywords t)
  (org-export-backends '(ascii html icalendar latex md))
  (org-export-coding-system 'utf-8)
  (org-html-checkbox-type 'html)
  (org-list-allow-alphabetical t)
  (org-log-done t)
  (org-log-reschedule "note")
  (org-log-into-drawer t)
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-refile-targets '(("inbox.org" :maxlevel . 2)
                        ("agenda.org" :maxlevel . 3)
                        ("articles.org" :maxlevel . 3)
                        ("notebook.org" :maxlevel . 5)
                        ("work.org" :maxlevel . 5)
                        ("personal.org" :maxlevel . 5)
                        (nil :maxlevel . 2)))
  (org-reverse-note-order t)
  (org-src-fontify-natively t)
  (org-startup-indented t)
  (org-startup-folded 'content)
  (org-timer-default-timer 25)
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

  :bind
  (("<f8>" . org-cycle-agenda-files)
   ("<f12>" . org-agenda)
   ("C-c l" . org-store-link)
   ("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c b" . org-switchb))
  (:map org-mode-map
        ("M-q" . endless/fill-or-unfill)
        ("RET" . org-return))

  :init
  (setq org-file-apps
        '((auto-mode . emacs)
          ("\\.doc\\'" . "open %s")
          ("\\.docx\\'" . "open %s")
          ("\\.xlsx\\'" . "open %s")
          ("\\.pptx\\'" . "open %s")
          ("\\.pdf\\'" . default)))
  (setq org-structure-template-alist
        '(("a" . "export ascii")
          ("c" . "center")
          ("C" . "comment")
          ("e" . "example")
          ("E" . "export")
          ("h" . "export html")
          ("l" . "src emacs-lisp")
          ("m" . "export md")
          ("p" . "src python")
          ("q" . "quote")
          ("s" . "src")
          ("v" . "verse")
          ("y" . "src yaml")))

  ;; Workflow states
  ;; https://orgmode.org/manual/Workflow-states.html#Workflow-states
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "SOMEDAY(m)" "|" "DONE(d)" "DELEGATED(l)" "CANCELLED(c)")
                            (sequence "PROJECT" "|" "DONE(d)")
                            (sequence "|" "MEETING" "REFERENCE(r)")))
  ;; List of tags that should never be inherited.
  (setq org-tags-exclude-from-inheritance '("crypt"))
  ;;
  (setq org-columns-default-format "%50ITEM(Task) %2PRIORITY %10Effort(Effort){:} %10CLOCKSUM"
        org-modules '(org-id ol-eww ol-docview ol-info ol-irc org-habit)
        org-src-preserve-indentation t
        org-src-window-setup 'current-window                    ;; use current window when editing a source block
        org-cycle-separator-lines 2                             ;; leave this many empty lines in collapsed view
        org-table-export-default-format "orgtbl-to-csv"         ;; export tables as CSV instead of tab-delineated
        org-publish-project-alist '(("public"
                                     :base-directory "~/forge"
                                     :publishing-directory "~/Documents")))

  )



(with-eval-after-load 'org
  ;; (forge/org-fixed-font-faces)
  ;; (org-load-modules-maybe t)
  (org-babel-do-load-languages 'org-babel-load-languages '((ditaa . t)
                                                           (dot . t)
                                                           (emacs-lisp . t)
                                                           (org . t)
                                                           (perl . t)
                                                           (python . t)
                                                           (ruby . t)
                                                           (shell . t)
                                                           (calc . t))))

(use-package ol-notmuch
  :after (:any org notmuch))

(use-package org-mime
  :config
  (add-hook 'message-mode-hook
            (lambda ()
              (local-set-key "\C-c\M-o" 'org-mime-htmlize)))
  (add-hook 'org-mode-hook
            (lambda ()
              (local-set-key "\C-c\M-o" 'org-mime-org-buffer-htmlize)))
  :init
  (setq org-mime-export-options '(:section-numbers nil :with-author nil :with-toc nil)))

(use-package org-contacts
  :after org
  :config
  (setq org-contacts-files (list  "~/forge/contacts.org"))
  (add-to-list 'org-capture-templates
               '("C" "Contacts" entry
                 (file "~/forge/contacts.org")
                 "* %(org-contacts-template-name)\n:PROPERTIES:\n:EMAIL: %(org-contacts-template-email)\n:PHONE:\n:ADDRESS:\n:BIRTHDAY:\n:END:")))

(use-package org-tree-slide
  :bind (:map org-tree-slide-mode-map
              ("<f8>" . org-tree-slide-mode)
              ("<f9>" . org-tree-slide-move-previous-tree)
              ("<f10>" . org-tree-slide-move-next-tree)
              ("C-,"  . org-tree-slide-move-previous-tree)
              ("C-." . org-tree-slide-move-next-tree))
  :init
  (setq org-tree-slide-skip-outline-level 4))

(use-package ox-reveal)

(use-package holidays
  :defer t
  :ensure nil
  :custom
  (holiday-bahai-holidays nil)
  (holiday-other-holidays
   '((holiday-fixed 6 19 "Juneteenth"))
   ))

(with-eval-after-load 'org-crypt
  (org-crypt-use-before-save-magic)
  (setq org-crypt-disable-auto-save t
        org-crypt-key user-full-name))

(with-eval-after-load 'org-id
  (setq org-id-method 'uuid
        org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
        org-id-locations-file (expand-file-name "org/id-locations.el" forge-state-dir)))

;;(use-package ol-git-link :straight (org-contrib :includes ol-git-link))

(use-package ol-eww
  :ensure nil
  :after org)

;; support links to manual pages
(use-package ol-man
  :ensure nil
  :after org)

(use-package ox-md
  :ensure nil
  :after org)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package htmlize)

;; (use-package ox-twbs
;;   :commands (org-twbs-export-to-html
;;              org-twbs-export-as-html
;;              org-twbs-convert-region-to-html))

;; (use-package ox-reveal
;;   :after org-compat
;;   :custom (org-reveal-note-key-char nil))

;; (use-package ox-tufte :after org)

(defun forge/capture-current-song ()
  "Capture the current song details."
  (let ((itunes-song (my-get-current-song-itunes))
        (mpd-song (when (fboundp 'forge/get-current-song-mpd) (forge/get-current-song-mpd)))
        (song-info nil))
    (setq song-info (if itunes-song itunes-song mpd-song))
    (concat (car song-info) ", \"" (car (cdr song-info)) "\"")))

(defun my-org-set-lastupdated ()
  "Set LASTUPDATED property to today."
  (interactive)
  (org-set-property "LASTUPDATED" (format-time-string (org-time-stamp-format nil t))))

(defun forge/org-table-export (name)
  "Search for table named `NAME` and export"
  (interactive "sTable: ")
  (outline-show-all)
  (push-mark)
  (goto-char (point-min))
  (let ((case-fold-search t))
    (if (search-forward-regexp (concat "#\\+NAME: +" name) nil t)
        (progn
          (forward-line)
          (org-table-export (format "%s.csv" name) "orgtbl-to-csv"))))
  (pop-mark))

;; via https://vxlabs.com/2018/10/29/importing-orgmode-notes-into-apple-notes/
(defun forge/org-html-publish-to-html-for-apple-notes (plist filename pub-dir)
  "Convert exported files to format that plays nicely with Apple Notes. Takes PLIST, FILENAME, and PUB-DIR."
  ;; https://orgmode.org/manual/HTML-preamble-and-postamble.html
  ;; disable author + date + validate link at end of HTML exports
  ;;(setq org-html-postamble nil)

  (let* ((org-html-with-latex 'imagemagick)
         (outfile
          (org-publish-org-to 'html filename
                              (concat "." (or (plist-get plist :html-extension)
                                              org-html-extension
                                              "html"))
                              plist pub-dir)))
    ;; 1. apple notes handles <p> paras badly, so we have to replace all blank
    ;;    lines (which the orgmode export accurately leaves for us) with
    ;;    <br /> tags to get apple notes to actually render blank lines between
    ;;    paragraphs
    ;; 2. remove large h1 with title, as apple notes already adds <title> as
    ;; the note title
    (shell-command (format "sed -i \"\" -e 's/^$/<br \\/>/' -e 's/<h1 class=\"title\">.*<\\/h1>$//' %s" outfile)) outfile))

(defun forge/tangle-file (file)
  "Given an 'org-mode' FILE, tangle the source code."
  (interactive "fOrg File: ")
  (find-file file)
  (org-babel-tangle)
  (kill-buffer))

(defun forge/tangle-files (path &optional full)
  "Tangle files in PATH (directory), FULL for absolute paths.
Example: (forge/tangle-files \"~/.emacs.d/*.org\")."
  (interactive)
  (mapc 'forge/tangle-file (forge/get-files path full)))

(defun forge/get-files (path &optional full)
  "Return list of files in directory PATH that match glob pattern, FULL for absolute paths."
  (directory-files (file-name-directory path)
                   full
                   (eshell-glob-regexp (file-name-nondirectory path))))

(defun my/migrate-datetree-entry ()
  "Take an org entry from a datetree outline and migrate to an org-journal file.

The general intent behind this function is that it will migrate the current heading
and advance to the next heading.  One can then bind it to a macro for the repetition piece.
It will not remove entries from the source org file."
  (interactive)
  (org-beginning-of-line)
  (let* ((heading (nth 4 (org-heading-components)))
         (tags (nth 5 (org-heading-components)))
         (year (format-time-string "%Y" (apply 'encode-time (org-parse-time-string heading))))
         (time (format-time-string "%H:%M" (apply 'encode-time (org-parse-time-string heading))))
         (day (format-time-string "%A, %d %B %Y" (apply 'encode-time (org-parse-time-string heading))))
         (subject (when (string-match "\] *\\(.*\\)" heading) (match-string 1 heading)))
         (day-heading (format "* %s" day))
         (jrnl-heading (format "** %s %s   %s" time subject (or tags ""))))
    (org-copy-subtree)
    (with-current-buffer year
      (my/migrate-datetree-goto-heading day-heading)
      (message "%s" jrnl-heading)
      (insert (format "%s\n" jrnl-heading))
      (org-paste-subtree)
      (kill-line 1))
    (forward-line)
    ;; go to next datetree heading
    (re-search-forward "^\\*\\*\\*\\* \\[" nil t)))

(defun my/migrate-journal-entry ()
  "Migrate an org-journal entry."
  (interactive)
  (org-beginning-of-line)
  (if (= 1 (nth 1 (org-heading-components)))
      (org-next-visible-heading 1)
    (let* ((heading (nth 4 (org-heading-components)))
           (time (when (string-match "\\(..:..\\)" heading) (match-string 1 heading))))
      (push-mark)
      (outline-up-heading 1)
      (let* ((date (org-entry-get (point) "CREATED"))
             (date-heading (nth 4 (org-heading-components)))
             (weekday (when (string-match "\\(.*\\)," date-heading) (match-string 1 date-heading)))
             (shortday (when (string-match "\\(...\\)" weekday) (match-string 1 weekday)))
             (month (when (string-match ", [0-9]+ \\(.*\\) " date-heading) (match-string 1 date-heading)))
             (year (when (string-match "\\(....\\)" date) (match-string 1 date)))
             (monthint (when (string-match "....\\(..\\).." date) (match-string 1 date)))
             (dayint (when (string-match "......\\(..\\)" date) (match-string 1 date)))
             (month-heading (format "** %s-%s %s" year monthint month))
             (day-heading (format "*** %s-%s-%s %s" year monthint dayint weekday))
             (org-ts (format "[%s-%s-%s %s %s]" year monthint dayint shortday time)))
        (pop-global-mark)
        (org-copy-subtree)
        (with-current-buffer "journal.org"
          (my/migrate-datetree-goto-heading month-heading)
          (message "month heading %s" month-heading)
          (my/migrate-datetree-goto-heading day-heading)
          (message "day heading %s" day-heading)
          (org-paste-subtree 4)
          (forward-line)
          (insert (format "%s\n" org-ts)))
        (org-next-visible-heading 1)))))

(defun my/migrate-datetree-goto-heading (heading)
  "Go to day heading HEADING in org-journal file.  Create if it doesn't exist."
  (interactive)
  (goto-char (point-min))
  (unless (search-forward heading nil t)
    (progn (goto-char (point-max))
           (insert (format "%s\n" heading))
           (goto-char (point-min))))
  (search-forward heading nil t)
  (goto-char (point-max)))

(provide 'init-org)
