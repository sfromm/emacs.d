;;; eos-mode.el --- edit Arista EOS configuration files -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Stephen Fromm <sfromm at gmail.com>
;; Copyright (C) 2004 Noufal Ibrahim <nkv at nibrahim.net.in>
;;
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
;; Based on ios-config-mode.el from https://github.com/nibrahim/IOS-config-mode
;; by Noufal Ibrahim.

;;; Code:

(require 'rx)

(defgroup eos-mode nil
  "EOS Mode"
  :group 'editing)

(defcustom eos-mode-hook nil
  "Hook called by \"eos-mode\"."
  :group 'eos-mode
  :type 'hook)

(defvar eos-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j") #'newline-and-indent)
    map)
  "Keymap for Arista router configuration major mode.")


;; Constants

(defconst eos-mode-version "0.2" "Version of `eos-mode'.")

(defconst eos-section-regex
  (concat
   "^ *"
   (regexp-opt
    '("controller" "hardware tcam" "interface" "ip routing vrf"
      "ip access-list" "ip prefix-list" "ipv6 prefix-list"
      "line" "management api" "policy-map" "redundancy" "route-map" "router") t)))

(defconst eos-keywords-regex
  (regexp-opt
   '("aaa" "description" "isis" "logging" "neighbor" "vlan") 'words)
  "Regular expressions for EOS keywords.")

(defconst eos-commands-regex
  (concat
   "^ *"
   (regexp-opt
    '("ip community-list" "ip radius" "ip routing" "ip route"
      "ntp server" "radius-server" "snmp-server") t))
  "Regular expressions for EOS commands.")

(defconst eos-ipaddr-regex
  (rx
   (or
    (group (repeat 1 3 digit) "\."
           (repeat 1 3 digit) "\."
           (repeat 1 3 digit) "\."
           (repeat 1 3 digit)
           (optional "/" (one-or-more digit)))
    (group "::/" (one-or-more digit))))
  "RegExp for an IP address.")

(defconst eos-no-regex
  (regexp-opt '("no") 'words)
  "Regular expression for `no' commands.")

(defconst eos-shutdown-regex
  (regexp-opt '("shutdown") 'words)
  "Regular expression for `shutdown' command.")

(defvar eos-font-lock-keywords
  (list
   (list eos-section-regex 0 font-lock-function-name-face)
   (list eos-keywords-regex 0 font-lock-keyword-face)
   (list eos-commands-regex 0 font-lock-builtin-face)
   (list eos-ipaddr-regex 0 font-lock-string-face)
   (list eos-no-regex 0 font-lock-warning-face)
   (list eos-shutdown-regex 0 font-lock-warning-face)
   )
  "Font locking definitions for Arista eos mode.")

(defun eos-indent-line ()
  "Indent current line as arista eos config line."
  (let ((indent0 "^interface\\|redundancy\\|^line\\|^ip vrf \\|^controller\\|^class-map\\|^policy-map\\|router\\|access-list\\|route-map")
        (indent1 " *main-cpu\\| *class\\W"))
    (beginning-of-line)
    (let ((not-indented t)
          (cur-indent 0))
      (cond ((or (bobp) (looking-at indent0) (looking-at "!")) ; Handles the indent0 and indent1 lines
             (setq not-indented nil
                   cur-indent 0))
            ((looking-at indent1)
             (setq not-indented nil
                   cur-indent 1)))
      (save-excursion ; Indents regular lines depending on the block they're in.
        (while not-indented
          (forward-line -1)
          (cond ((looking-at indent1)
                 (setq cur-indent 2
                       not-indented nil))
                ((looking-at indent0)
                 (setq cur-indent 1
                       not-indented nil))
                ((looking-at "!")
                 (setq cur-indent 0
                       not-indented nil))
                ((bobp)
                 (setq cur-indent 0
                       not-indented nil)))))
      (indent-line-to cur-indent))))


;; Mode setup

;; Imenu definitions.
(defvar eos-imenu-expression
  '(
    ("Interfaces"        "^[\t ]*interface *\\(.*\\)" 1)
    ("VRFs"              "^ *vrf *\\(.*\\)" 1)
    ("Controllers"       "^[\t ]*controller *\\(.*\\)" 1)
    ("Routing protocols" "^router *\\(.*\\)" 1)
    )
  "Imenu configuration for `eos-mode'.")

(defvar eos-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" eos-mode-syntax-table)  ;; All _'s are part of words.
    (modify-syntax-entry ?- "w" eos-mode-syntax-table)  ;; All -'s are part of words.
    (modify-syntax-entry ?! "<" eos-mode-syntax-table)  ;; All !'s start comments.
    (modify-syntax-entry ?\n ">" eos-mode-syntax-table) ;; All newlines end comments.
    (modify-syntax-entry ?\r ">" eos-mode-syntax-table) ;; All linefeeds end comments.
    syntax-table)
  "Syntax table for Arista `eos-mode'.")

;;;###autoload
(define-derived-mode eos-mode text-mode "EOS"
  "Major mode for Arista EOS (TM) configuration files"
  :syntax-table eos-mode-syntax-table
  :group 'eos-mode
  (set (make-local-variable 'font-lock-defaults) '(eos-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'eos-indent-line)
  (set (make-local-variable 'comment-start) "!")
  (set (make-local-variable 'comment-start-skip) "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)!+ *")
  (setq imenu-case-fold-search nil)
  (set (make-local-variable 'imenu-generic-expression) eos-imenu-expression)
  (imenu-add-to-menubar "Imenu"))

(defun eos-mode-version ()
  "Display version of `eos-mode'."
  (interactive)
  (message "eos-mode %s" eos-mode-version)
  eos-mode-version)

(provide 'eos-mode)

;;; eos-mode.el ends here
