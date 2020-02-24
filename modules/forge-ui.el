;;; forge-ui.el --- Configure UI elements.  -*- lexical-binding: t -*-

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


(setq scroll-step 1               ;; keyboard scroll one line at a time
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      mouse-wheel-progressive-speed nil     ;;  don't accelerate scrolling
      mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))


;;;
;;; which-key
;;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :defer 5
  :diminish
  :commands which-key-mode
  :config (which-key-mode))


;;;
;;; ivy, swiper, and counsel
;;; https://github.com/abo-abo/swiper
(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :bind
  (("C-c C-r" . ivy-resume))
  :init
  (ivy-mode 1)
  :config
  (define-key ivy-minibuffer-map (kbd "<tab>") 'ivy-alt-done)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t))

(use-package swiper
  :ensure t
  :diminish
  :bind (("C-s" . swiper-isearch)))

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
;;; https://github.com/nonsequitur/smex/
(use-package smex
  :ensure t
  :init
  (setq smex-completion-method 'ivy
        smex-save-file (concat forge-state-dir "smex-items")))

;;; ace-window
;;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :ensure t
  :custom (aw-scope 'frame)
  :bind ([remap other-window] . ace-window))

;;; avy
;;; https://github.com/abo-abo/avy
(use-package avy
  :ensure t
  :bind
  (("M-g g" . avy-goto-line)
   ("M-s" . avy-goto-word-1)))

;;; dump-jump
;;; https://github.com/jacktasia/dumb-jump
(use-package dumb-jump
  :defer t
  :ensure t
  :bind
  (("M-g o" . dumb-jump-go-other-window)
   ("M-g j" . dumb-jump-go)
   ("M-g i" . dumb-jump-go-prompt)
   ("M-g b" . dumb-jump-back)
   ("M-g q" . dumb-jump-quick-look)
   ("M-g x" . dumb-jump-go-prefer-external)
   ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy))


;;;
;;; eyebrowse
;;; https://github.com/wasamasa/eyebrowse
(use-package eyebrowse
  :defer t
  :custom (eyebrowse-keymap-prefix (kbd "C-\\"))
  :ensure t
  :bind
  (("M-1" . eyebrowse-switch-to-window-config-1)
   ("M-2" . eyebrowse-switch-to-window-config-2)
   ("M-3" . eyebrowse-switch-to-window-config-3)
   ("M-4" . eyebrowse-switch-to-window-config-4))
  :config
  (eyebrowse-mode 1))


;;;
;;; golden ratio for windows
;;; https://github.com/roman/golden-ratio.el
(use-package golden-ratio
  :defer t
  :config
  (setq golden-ratio-exclude-modes '(messages-buffer-mode
                                     fundamental-mode
                                     ediff-mode
                                     calendar-mode
                                     calc-mode
                                     calc-trail-mode
                                     magit-popup-mode))
  (add-to-list 'golden-ratio-extra-commands 'ace-window))


;;;
;;; uniquify
;;;
(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward
              uniquify-separator "/"
              uniquify-ignore-buffers-re "^\\*"
              uniquify-after-kill-buffer-p t))


;;;
;;; olivetti - emacs minor mode for a writing environment
;;; https://github.com/rnkn/olivetti
(use-package olivetti
  :ensure t
  :custom (olivetti-hide-mode-line t)
  :commands olivetti-mode)

(defun forge-focus ()
  "Enable features to focus."
  (interactive)
  (olivetti-mode))


;;;
;;; hydra
;;; https://github.com/abo-abo/hydra
(use-package hydra :ensure t)

(defhydra forge/navigate (:foreign-keys run)
  "[Navigate] or q to exit."
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
  ("[" backward-page)
  ("]" forward-page)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("." end-of-buffer)
  ("C-'" nil)
  ("q" nil :exit t))

(defhydra forge/window ()
  ("a" ace-window "Ace Window" :exit t)
  ("t" transpose-frame "Transpose" :exit t)
  ("o" ace-delete-other-windows "Delete other windows " :exit t)
  ("s" ace-swap-window "Swap window" :exit t)
  ("d" ace-delete-window "Delete window" :exit t)
  ("b" ivy-switch-buffer "Switch" :exit t)
  ("g" golden-ratio "Golden ratio" :exit t)
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)) "Split Vert")
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)) "Split Horz")
  ("q" nil))
(global-set-key (kbd "C-c n") 'forge/navigate/body)


;;;
;;; Helper for my mkhome makefile
;;;
(defmacro forge-mkhome-target (target)
  "Macro to run mkhome makefile TARGET."
  `(with-temp-buffer
     (progn
       (cd (getenv "HOME"))
       (compile (mapconcat 'shell-quote-argument (list "make" "-f" "Makefile.mkhome" ,target) " ")))))

(defun forge-mkhome-git ()
  "Run mkhome git."
  (interactive)
  (forge-mkhome-target "git"))

(define-prefix-command 'forge-mkhome-map)
(define-key forge-mkhome-map (kbd "g") 'forge-mkhome-git)


;;;
;;; forge keymap
(define-prefix-command 'forge-map)
(define-key forge-map (kbd "w") 'forge/window/body)
(define-key forge-map (kbd "n") 'forge/navigate/body)
(define-key forge-map (kbd "m") 'notmuch-cycle-notmuch-buffers)
(define-key forge-map (kbd "h") 'forge-mkhome-map)
(define-key forge-map (kbd "f") 'elfeed)
(define-key forge-map (kbd "j") 'forge/jabber-start-or-switch)
(define-key forge-map (kbd "g") 'magit-status)
(define-key forge-map (kbd "s") 'eshell-here)
(define-key forge-map (kbd "S") 'forge/slack/body)
(define-key forge-map (kbd "p") 'paradox-list-packages)
(define-key forge-map (kbd "u") 'browse-url-at-point)
(define-key forge-map (kbd "F") 'forge-focus)
(global-set-key (kbd "C-z") 'forge-map)



;;;
;;;
;;;
(provide 'forge-ui)
;;; forge-ui.el ends here
