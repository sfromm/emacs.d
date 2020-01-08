;;; forge-orgmode.el --- This is an Emacs Lisp file with Emacs Lisp code. -*- lexical-binding: t -*-

;; Copyright (C) Stephen Fromm

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

(use-package org
  :ensure org-plus-contrib
  :preface
  (defun forge/capture-current-song ()
    "Capture the current song details."
    (let ((itunes-song (forge/get-current-song-itunes))
          (mpd-song (when (fboundp 'forge/get-current-song-mpd) (forge/get-current-song-mpd)))
          (song-info nil))
      (setq song-info (if itunes-song itunes-song mpd-song))
      (concat (car song-info) ", \"" (car (cdr song-info)) "\"")))

  (defun forge/org-tbl-export (name)
    "Search for table named `NAME` and export"
    (interactive "s")
    (outline-show-all)
    (push-mark)
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (search-forward-regexp (concat "#\\+NAME: +" name) nil t)
          (progn
            (next-line)
            (org-table-export (format "%s.csv" name) "orgtbl-to-csv"))))
    (pop-mark))

  (defun forge/tangle-org-mode-on-save ()
    "Tangle org-mode file when saving."
    (when (string= (message "%s" major-mode) "org-mode")
      (org-babel-tangle)))

  (defun forge/org-mode-hook ()
    "Turn on settings for org-mode."
    (interactive)
    (set-fill-column 100)
    (when (fboundp 'turn-on-auto-fill)
      (turn-on-auto-fill))
    (when (fboundp 'turn-on-flyspell)
      (turn-on-flyspell)))

  (defun forge/org-set-uuid ()
    "Set ID property for current headline."
    (interactive)
    (org-set-property "ID" (org-id-uuid)))

  (defun forge/org-set-created ()
    "Set CREATED property for current headline."
    (interactive)
    (org-set-property "CREATED" (with-temp-buffer (org-insert-time-stamp (current-time) t t))))

  (defun forge/org-set-properties ()
    "Set stock org properties for current headline."
    (interactive)
    (forge/org-set-uuid)
    (forge/org-set-created))

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

  :hook
  ((org-mode . forge/org-mode-hook)
   (after-save . forge/tangle-org-mode-on-save)
   (org-mode . variable-pitch-mode))

  :custom
  (org-refile-use-outline-path 'file)
  (org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))

  :bind (("<f8>" . org-cycle-agenda-files)
         ("<f12>" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-switchb))
  :bind (:map org-mode-map
              ("M-q" . endless/fill-or-unfill)
              ("RET" . org-return-indent))
  :init
  (setq org-directory "~/forge"
        org-default-notes-file (concat org-directory "/journal.org")
        org-file-apps (quote ((auto-mode . emacs)
                              ("\\.doc\\'" . "open %s")
                              ("\\.docx\\'" . "open %s")
                              ("\\.xlsx\\'" . "open %s")
                              ("\\.pptx\\'" . "open %s")
                              ("\\.pdf\\'" . default))))
  (setq org-agenda-sticky t
        org-agenda-restore-windows-after-quit t
        org-agenda-window-setup 'current-window
        org-agenda-files (list
                          (concat org-directory "/journal.org")
                          (concat org-directory "/tasks.org")
                          (concat org-directory "/work.org")
                          (concat org-directory "/personal.org")
                          (concat org-directory "/notebook.org"))
        org-agenda-custom-commands '(("i" "Inbox"
                                      ((tags-todo "REFILE"
                                                  ((org-agenda-overriding-header "Inbox")))))
                                     ("P" "Projects"
                                      ((todo "PROJECT"
                                             ((org-agenda-overriding-header "Projects")))))))

  ;; For template expansion,
  ;; see https://orgmode.org/manual/Template-expansion.html#Template-expansion
  (setq org-capture-templates '(("l" "Log" entry (file+olp+datetree "~/forge/journal.org") "* %U - %?\n" )

                                ("j" "Journal" entry (file+olp+datetree "~/forge/journal.org") "* %?\n%U\n" )

                                ("b" "Bookmark" entry (file+olp+datetree "~/forge/journal.org")
                                 "* [[%^{Link}][%^{Description}]] :bookmark: \n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :prepend t)

                                ("t" "To do" entry (file+headline "~/forge/journal.org" "Inbox")
                                 "* TODO %? :inbox:\n:PROPERTIES:\n:ID:       %(shell-command-to-string \"uuidgen\"):CREATED:  %U\n:END:\nReference: %a\n" :prepend t)

                                ("m" "Music" entry (file+olp+datetree "~/forge/journal.org")
                                 "* %(forge/capture-current-song) :music:\n%U\n")))

  (setq org-clock-out-remove-zero-time-clocks t
        org-ellipsis "⤵"
        org-log-done t

        org-log-reschedule "note"
        org-export-allow-bind-keywords t
        org-export-coding-system 'utf-8

        org-modules '(org-w3m org-bbdb org-bibtex org-docview
                              org-gnus org-info org-irc org-mhe org-rmail org-habit)
        org-src-preserve-indentation t
        org-src-window-setup 'current-window                    ;; use current window when editing a source block
        org-cycle-separator-lines 2                             ;; leave this many empty lines in collapsed view
        org-table-export-default-format "orgtbl-to-csv"         ;; export tables as CSV instead of tab-delineated
        org-todo-keywords '((sequence "TODO(t)" "PROJECT(p)" "WAITING(w)" "SOMEDAY(m)" "|" "DONE(d)" "DELEGATED(l)" "CANCELLED(c)"))
        org-publish-project-alist '(("public"
                                     :base-directory "~/forge"
                                     :publishing-directory "~/Documents")))
  (org-babel-do-load-languages 'org-babel-load-languages '((ditaa . t)
                                                           (emacs-lisp . t)
                                                           (org . t)
                                                           (perl . t)
                                                           (python . t)
                                                           (ruby . t)
                                                           (shell . t)
                                                           (calc . t)))

  ;; Keep tables with a fixed-pitch font.
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (org-load-modules-maybe t))




(use-package org-git-link)

(use-package org-contacts
    :after org
    :config
    (setq org-contacts-files (list  "~/forge/contacts.org"))
    (add-to-list 'org-capture-templates '("c" "Contacts" entry
                                          (file "~/forge/contacts.org")
                                          "* %(org-contacts-template-name)\n:PROPERTIES:\n:EMAIL: %(org-contacts-template-email)\n:PHONE:\n:ADDRESS:\n:BIRTHDAY:\n:END:")))

(use-package org-bullets
    :ensure t
    :after org
    :init (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package org-id
    :after org
    :config
    (setq org-id-method 'uuid
	  org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

(use-package org-indent
    :diminish t
    :config
    (setq org-startup-indented t))

(use-package ox-twbs
    :ensure t)

(use-package org-mime
    :ensure nil
    :defer t
    :hook
    (message-mode . (lambda () (local-set-key "\C-c\M-o" 'org-mime-htmlize)))
    :init
    (setq org-mime-export-options '(:section-numbers nil
                                    :with-author nil
                                    :with-toc nil)))

(use-package ox-reveal
    :ensure t
    :init
    (setq org-reveal-note-key-char nil))

(use-package ox-tufte
    :ensure t)


;;;
;;; org-journal
;;; https://github.com/bastibe/org-journal
(use-package org-journal
  :ensure t
  :preface
  (defun org-journal-find-location ()
    "Open today's journal file."
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    ;; This should also get org-mode to the right place to add a heading at the correct depth
    (org-journal-new-entry t)
    (goto-char (point-max)))
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  ;; (goto-char (point-max)))

  :init
  (setq org-journal-dir (concat org-directory "/journal/")
        org-journal-file-type 'yearly
        org-journal-file-format "%Y"
        org-journal-date-format "%A, %d %B %Y"))



(use-package org-tree-slide
  :ensure t
  :bind (:map org-tree-slide-mode-map
              ("<f8>" . org-tree-slide-mode)
              ("<f9>" . org-tree-slide-move-previous-tree)
              ("<f10>" . org-tree-slide-move-next-tree))
  :init
  (setq org-tree-slide-skip-outline-level 4))



(use-package org-pomodoro
  :ensure t
  :bind
  (("C-c C-x C-i" . org-pomodoro)
   ("C-c C-x C-o" . org-pomodoro))

  :preface
  (defun forge/notify-pomodoro (title message)
    (notifications-notify
     :title title
     :body message
     :urgency 'low))

  :hook
  (org-pomodoro-finished . (lambda () (forge/notify-pomodoro "Pomodoro completed" "Time for a break")))
  (org-pomodoro-break-finished . (lambda () (forge/notify-pomodoro "Break completed" "Ready for another?")))
  (org-pomodoro-long-break-finished . (lambda () (forge/notify-pomodoro "Long break completed" "Ready for another?")))

  :init
  (setq
   org-pomodoro-audio-player "mpv"
   org-pomodoro-finished-sound "~/annex/Music/drip.ogg"))


;;;
;;; Misc functions related to org life.
;;;
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


(provide 'forge-orgmode)
;;; forge-orgmode.el ends here
