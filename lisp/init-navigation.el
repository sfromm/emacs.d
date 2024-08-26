;;; init-navigation.el --- Init navigation elements -*- lexical-binding: t -*-
;; Navigation elements
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

(use-package view
  :bind
  (:map view-mode-map
        ("h" . backward-char)
        ("l" . forward-char)
        ("j" . next-line)
        ("k" . previous-line))
  :hook
  (view-mode . hl-line-mode)
  :config
  (cond ((derived-mode-p 'org-mode)
         (keymap-set view-mode-map "p" #'org-previous-visible-heading)
         (keymap-set view-mode-map "n" #'org-next-visible-heading))
        ((derived-mode-p 'elfeed-show-mode)
         (keymap-set view-mode-map "p" #'backward-paragraph)
         (keymap-set view-mode-map "n" #'forward-paragraph))
        ((derived-mode-p 'makefile-mode)
         (keymap-set view-mode-map "p" #'makefile-previous-dependency)
         (keymap-set view-mode-map "n" #'makefile-next-dependency))
        ((derived-mode-p 'emacs-lisp-mode)
         (keymap-set view-mode-map "p" #'backward-sexp)
         (keymap-set view-mode-map "n" #'forward-sexp))
        (t
         (define-key view-mode-map (kbd "p") 'scroll-down-command)
         (define-key view-mode-map (kbd "n") 'scroll-up-command))))

;;; windmove
(use-package windmove
  :bind
  (("s-l" . windmove-right)
   ("s-h" . windmove-left)
   ("s-k" . windmove-up)
   ("s-j" . windmove-down))
  :custom (windmove-wrap-around t)
  :config (windmove-default-keybindings 'super))

;;; ace-window
(use-package ace-window
  :bind
  (([remap other-window] . ace-window)))

(use-package dumb-jump
  :commands (xref-find-definitions)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;; this requires at least xref-1.1.0, which comes with emacs-28.1 or newer
  (when (version<= "28.1" emacs-version)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)))

(use-package avy
  :bind ("C-;" . avy-goto-char-timer)
  :custom (avy-case-fold-search t)
  :functions (avy-setup-default)
  :config (avy-setup-default))

(provide 'init-navigation)
