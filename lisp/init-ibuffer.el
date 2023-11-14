;;; init-ibuffer.el --- Init ibuffer -*- lexical-binding: t -*-
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


(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :preface
  (defun init-ibuffer-filters ()
    (ibuffer-switch-to-saved-filter-groups "default"))
  :hook
  (ibuffer-mode . init-ibuffer-filters)
  :custom
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-saved-filter-groups
   '(("default"
      ("Notmuch"
       (or
        (name . "\\*notmuch-")
        (mode . notmuch-search-mode)
        (mode . notmuch-show-mode)
        (mode . notmuch-message-mode)
        (mode . message-mode)))
      ("Org"
       (or
        (name . "^\\*Calendar\\*$")
        (name . "^\\*Org Agenda")
        (name . "^ \\*Agenda")
        (name . "^diary$")
        (mode . org-mode)))
      ("RANCID"
       (or
        (filename . "-rancid/")
        (mode . eos-mode)))
      ("DNS"
       (or
        (mode . dns-mode)
        (filename . "dns-zones/")))
      ("Lisp"
       (mode . emacs-lisp-mode))
      ("Dired" (mode . dired-mode))
      ("SSH"   (filename . "^/ssh:*"))
      ("Docker"
       (or
        (mode . dockerfile-mode)
        (mode . docker-compose-mode)))
      ("Magit"
       (or
        (mode . magit-status-mode)
        (mode . magit-log-mode)
        (name . "\\*magit")
        (name . "magit-")
        (name . "git-monitor")))
      ("Slack"
       (or
        (name . "^\\*Slack Log")
        (name . "^\\*Slack Event")
        (mode . slack-message-buffer-mode)
        (mode . slack-mode)))
      ("Commands"
       (or
        (mode . shell-mode)
        (mode . eshell-mode)
        (mode . term-mode)
        (mode . compilation-mode)))
      ("Emacs"
       (or
        (filename . ".emacs.d/emacs.org")
        (name . "^\\*scratch\\*$")
        (name . "^\\*Messages\\*$")
        (name . "^\\*\\(Customize\\|Help\\)")
        (name . "\\*\\(Echo\\|Minibuf\\)")))))))

(use-package ibuffer-vc
  :after (ibuffer vc)
  :bind (:map ibuffer-mode-map
              ("/ V" . ibuffer-vc-set-filter-groups-by-vc-root)))

(provide 'init-ibuffer)
