;;; forge-ui.el --- Configure UI elements.  -*- lexical-binding: t -*-

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

;;;
;;; ivy, swiper, and counsel
;;;
(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :bind
  (("C-c C-r" . ivy-resume))
  :init
  (ivy-mode 1)
  :config
  (define-key ivy-minibuffer-map (kbd "<tab>") 'ivy-alt-done)
  (setq
    ivy-use-virtual-buffers t
    enable-recursive-minibuffers t))

(use-package swiper
  :ensure t
  :diminish
  :bind (("C-s" . swiper)))

(use-package counsel
  :ensure t
  :requires ivy
  :bind
  (("C-c f" . counsel-git)
    ("M-x" . counsel-M-x)
    ("C-x C-f" . counsel-find-file))
  :config
  (setq ivy-use-virtual-buffers t))

;;;
;;; smex
;;;
(use-package smex
  :ensure t
  :init (setq smex-completion-method 'ivy))

(use-package ace-window
  :ensure t)

(use-package avy
  :ensure t
  :bind
  (("M-g g" . avy-goto-line)
    ("M-s" . avy-goto-word-1)))

(use-package dumb-jump
  :bind
  (("M-g o" . dumb-jump-go-other-window)
    ("M-g j" . dumb-jump-go)
    ("M-g i" . dumb-jump-go-prompt)
    ("M-g x" . dumb-jump-go-prefer-external)
    ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy))

;;;
;;; hydra
;;;
(use-package hydra
  :ensure t)

(defhydra forge/navigate (:foreign-keys run)
  "[Navigate] or q to exit"
  ("a" beginning-of-line)
  ("e" end-of-line)
  ("l" forward-char)
  ("h" backward-char)
  ("n" next-line)
  ("j" next-line)
  ("p" previous-line)
  ("k" previous-line)
  ("d" View-scroll-half-page-forward)
  ("u" View-scroll-half-page-backward)
  ("SPC" scroll-up-command)
  ("S-SPC" scroll-down-command)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("." end-of-buffer)
  ("C-'" nil)
  ("q" nil :exit t))

(global-set-key (kbd "C-c n") 'forge/navigate/body)

(provide 'forge-ui)
;;; forge-ui.el ends here
