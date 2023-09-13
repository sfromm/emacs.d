;;; notmuch-calendar.el -- Parse emails for calendar events and provide something for org-capture -*- lexical-binding: t; -*-

;; Author: Stephen Fromm
;; URL: NA
;; Package-Requires: ((emacs "24.1") (icalendar "0.19") (notmuch "0.20"))
;; Keywords: notmuch icalendar
;; Version: 0.1

;; This program is not part of GNU Emacs
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
;;
;;; Commentary:
;;
;; The goal is to capture emails with text/calendar mime-types to an
;; org-mode heading with salient details present.  The format is presently
;; hard-coded.
;;
;; This is strongly geared to using something like the following for capturing:
;;
;; (setq org-capture-templates '(("c" "Calendar Invite" entry
;;                                (file+olp+datetree "agenda.org")
;;                                (function notmuch-calendar-capture-event) :prepend t)))
;;
;; Inspiration from:
;; https://github.com/larkery/emacs/blob/master/site-lisp/notmuch-agenda.el
;; https://github.com/larkery/emacs-bankruptcy/blob/master/site-lisp/notmuch-calendar-x.el
;; https://github.com/vjohansen/emacs-config/blob/master/org-import-calendar.el
;;
;;; Code:

(require 'icalendar)
(require 'org-id)
(require 'mm-decode)
(require 'notmuch-show)
(require 'notmuch)

;;;###autoload
(defun notmuch-calendar-capture-event ()
  "Parse message body for calendar details and return string for `org-capture'."
  (interactive)
  (let ((notmuch-id (notmuch-show-get-message-id))
        (notmuch-from (notmuch-show-get-from))
        (notmuch-subject (notmuch-show-get-subject))
        (result))
    (with-current-notmuch-show-message
     (let ((mm-handle (mm-dissect-buffer)))
       (message "Parsing message body...")
       (notmuch-foreach-mime-part
        (lambda (p)
          (let* ((mm-type (mm-handle-type p))
                 (ical-event (notmuch-calendar-parse-calendar-part mm-type p)))
            (when (equal (car mm-type) "text/calendar")
              (setq result (notmuch-calendar-org-heading ical-event notmuch-id notmuch-from notmuch-subject)))))
        mm-handle)))
    (message "capture template for troubleshooting purposes: %s" result)
    result))

;;;###autoload
(defun notmuch-calendar-org-heading (ical-event notmuch-id notmuch-from notmuch-subject)
  "With provided ICAL-EVENT, NOTMUCH-ID, NOTMUCH-FROM, and NOTMUCH-SUBJECT, return an `org-mode' heading."
  (concat "* " notmuch-subject " %?\n"
          ":PROPERTIES:\n"
          ":CAPTURED:    %U\n"
          ":ID:        " (org-id-uuid) "\n"
          ":ORGANIZER: [[" (cadr (assoc 'organizer ical-event)) "]]\n"
          ":LOCATION:  " (cadr (assoc 'location ical-event)) "\n"
          ":END:\n"
          (notmuch-calendar-org-date (cadr (assoc 'dtstart ical-event))
                                     (cadr (assoc 'dtend ical-event))
                                     (cadr (assoc 'rrule ical-event)))
          "\n"
          "%a\n"
          ))

;;;###autoload
(defun notmuch-calendar-parse-calendar-part (mm-type mm-part)
  "Parse a message part MM-PART text/calendar with message type MM-TYPE."
  (when (equal (car mm-type) "text/calendar")
    (mm-with-unibyte-buffer
      (mm-insert-part mm-part)
      (let* ((ical-contents (icalendar--read-element nil nil))
             (events (icalendar--all-events ical-contents))
             (zone-map (icalendar--convert-all-timezones events))
             (ical-event))
        (dolist (event events)
          (setq ical-event (notmuch-calendar-parse-event event zone-map)))
        ical-event))))

;;;###autoload
(defun notmuch-calendar-parse-event (event zone-map)
  "Parse a calendar event EVENT in timezone ZONE-MAP and return something."
  (interactive)
  (let* ((summary (icalendar--get-event-property event 'SUMMARY))
         (dtstart (notmuch-calendar-parse-event-time event zone-map 'DTSTART))
         (dtend (notmuch-calendar-parse-event-time event zone-map 'DTEND))
         (rrule (icalendar--get-event-property event 'RRULE))
         (rdate (icalendar--get-event-property event 'RDATE))
         (location (icalendar--get-event-property event 'LOCATION))
         (organizer (icalendar--get-event-property event 'ORGANIZER))
         (attendees (icalendar--get-event-properties event 'ATTENDEE)))
    (list (list 'summary summary)
          (list 'dtstart dtstart)
          (list 'dtend dtend)
          (list 'rrule rrule)
          (list 'rdate rdate)
          (list 'location location)
          (list 'organizer organizer)
          (list 'attendees attendees))))

;;;###autoload
(defun notmuch-calendar-org-date (dtstart dtend rrule)
  "Return an Org-formatted date.  Arguments are DTSTART DTEND RRULE as returned by `notmuch-calendar-parse-event'."
  (let* ((start-d (datetime-to-iso dtstart))
         (start-t (icalendar--datetime-to-colontime dtstart))
         (end-d (if dtend (datetime-to-iso dtend) start-d))
         (end-t (if dtend (icalendar--datetime-to-colontime dtend) start-t)))
    (if (equal start-d end-d)
        (format "<%s %s-%s>" start-d start-t end-t)
      (format "<%s %s>--<%s %s>" start-d start-t end-d end-t))))

;;;###autoload
(defun notmuch-calendar-parse-event-time (event zone-map property)
  "Return ISODATETIME in format like `decode-time' from calendar EVENT, ZONE-MAP, and PROPERTY."
  (interactive)
  (let* ((timestamp (icalendar--get-event-property event property))
         (zone (icalendar--find-time-zone
                (icalendar--get-event-property-attributes event property) zone-map)))
    (icalendar--decode-isodatetime timestamp nil zone)))

;;;###autoload
(defun datetime-to-iso (datetime)
  "Convert datetime format DATETIME from `decode-time' to Org ISO YYYY-MM-DD."
  (if datetime
      (format "%04d-%02d-%02d"
              (nth 5 datetime)                  ; Year
              (nth 4 datetime)                  ; Month
              (nth 3 datetime))))

(provide 'notmuch-calendar)
;;; notmuch-calendar.el ends here
