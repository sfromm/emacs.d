;;; eos-mode.el --- edit Arista EOS configuration files

;; Copyright (C) 2020 Stephen Fromm <sfromm at gmail.com>
;; Copyright (C) 2004 Noufal Ibrahim <nkv at nibrahim.net.in>
;;
;; This program is not part of Gnu Emacs
;;
;; eos-mode.el is free software; you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either
;; version 2 of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Notes:
;; Based on ios-config-mode.el from https://github.com/nibrahim/IOS-config-mode

;;; Code:

(defgroup eos-mode nil
  "EOS Mode"
  :group 'editing)

(defcustom eos-mode-hook nil
  "Hook called by \"eos-mode\""
  :group 'eos-mode
  :type 'hook)

(defvar eos-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j") #'newline-and-indent)
    map)
  "Keymap for Arista router configuration major mode.")

;; Font locking definitions.
(defvar eos-command-face 'eos-command-face "Face for basic router commands")
(defvar eos-toplevel-face 'eos-toplevel-face "Face for top level commands")
(defvar eos-no-face 'eos-no-face "Face for \"no\"")
(defvar eos-ipadd-face 'eos-ipadd-face "Face for IP addresses")

(defface eos-ipadd-face
  '(
    (((type tty) (class color)) (:foreground "yellow"))
    (((type graphic) (class color)) (:foreground "LightGoldenrod"))
    (t (:foreground "LightGoldenrod" ))
    )
  "Face for IP addresses")

(defface eos-command-face
  '(
    (((type tty) (class color)) (:foreground "cyan"))
    (((type graphic) (class color)) (:foreground "cyan"))
    (t (:foreground "cyan" ))
    )
  "Face for basic router commands")

(defface eos-toplevel-face
  '(
    (((type tty) (class color)) (:foreground "blue"))
    (((type graphic) (class color)) (:foreground "lightsteelblue"))
    (t (:foreground "lightsteelblue" ))
    )
  "Face for basic router commands")

(defface eos-shutdown-face
  '(
    (((type tty) (class color)) (:foreground "red"))
    (((type graphic) (class color)) (:foreground "red"))
    (t (:foreground "red" ))
    )
  "Face for shutdown commands")

(defface eos-no-face
  '(
    (t (:underline t))
    )
  "Face for \"no\"")


;; (regexp-opt '("interface" "ip vrf" "controller" "class-map" "redundancy" "line" "policy-map" "router" "access-list" "route-map") t)
;; (regexp-opt '("diagnostic" "hostname" "logging" "service" "alias" "snmp-server" "boot" "card" "vtp" "version" "enable") t)

(defconst eos-font-lock-keywords
  (list
   '( "\\<\\(access-list\\|c\\(?:lass-map\\|ontroller\\)\\|i\\(?:nterface\\|p vrf\\)\\|line\\|policy-map\\|r\\(?:edundancy\\|oute\\(?:-map\\|r\\)\\)\\)\\>". eos-toplevel-face)
   '( "\\<\\(alias\\|boot\\|card\\|diagnostic\\|^enable\\|hostname\\|logging\\|s\\(?:ervice\\|nmp-server\\)\\|v\\(?:ersion\\|tp\\)\\)\\>" . eos-command-face)
   '("\\<\\(no\\)\\>" . eos-no-face)
   '("\\<\\(shutdown\\)\\>" . eos-shutdown-face)
   '("\\<\\([0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\(?:/[0-9]\\+\\)\\)\\>" . eos-ipadd-face)
   )
  "Font locking definitions for Arista eos mode")

;; Imenu definitions.
(defvar eos-imenu-expression
  '(
    ("Interfaces"        "^[\t ]*interface *\\(.*\\)" 1)
    ("VRFs"              "^ip vrf *\\(.*\\)" 1)
    ("Controllers"       "^[\t ]*controller *\\(.*\\)" 1)
    ("Routing protocols" "^router *\\(.*\\)" 1)
    ("Class maps"        "^class-map *\\(.*\\)" 1)
    ("Policy maps"       "^policy-map *\\(.*\\)" 1)
    ))

;; Indentation definitions.
(defun eos-indent-line ()
  "Indent current line as arista eos config line"
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


;; Custom syntax table
(defvar eos-mode-syntax-table (make-syntax-table)
  "Syntax table for Arista eos mode")

(modify-syntax-entry ?_ "w" eos-mode-syntax-table) ;All _'s are part of words.
(modify-syntax-entry ?- "w" eos-mode-syntax-table) ;All -'s are part of words.
(modify-syntax-entry ?! "<" eos-mode-syntax-table) ;All !'s start comments.
(modify-syntax-entry ?\n ">" eos-mode-syntax-table) ;All newlines end comments.
(modify-syntax-entry ?\r ">" eos-mode-syntax-table) ;All linefeeds end comments.

;;;###autoload
(define-derived-mode eos-mode c-mode "EOS"
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


(provide 'eos-mode)
;;; eos-mode.el ends here
