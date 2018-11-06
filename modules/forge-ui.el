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


(setq scroll-step 1               ;; keyboard scroll one line at a time
      scroll-conservatively 10000
      scroll-preserve-screen-position 1
      mouse-wheel-follow-mouse 't ;; scroll window under mouse
      mouse-wheel-progressive-speed nil     ;;  don't accelerate scrolling
      mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))


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
    :ensure t
    :bind ([remap other-window] . ace-window))

(use-package avy
    :ensure t
    :bind
    (("M-g g" . avy-goto-line)
     ("M-s" . avy-goto-word-1)))

(use-package dumb-jump
    :defer t
    :bind
    (("M-g o" . dumb-jump-go-other-window)
     ("M-g j" . dumb-jump-go)
     ("M-g i" . dumb-jump-go-prompt)
     ("M-g x" . dumb-jump-go-prefer-external)
     ("M-g z" . dumb-jump-go-prefer-external-other-window))
    :config
    (setq dumb-jump-selector 'ivy))


;;;
;;; golden ratio for windows
;;;
(use-package golden-ratio
    :defer t
    :config
    (append golden-ratio-extra-commands '(ace-window ace-delete-window ace-select-window
                                          ace-swap-window ace-maximize-window)))



;;;
;;; uniquify
;;;
(use-package uniquify
    :init (setq uniquify-buffer-name-style 'forward
                uniquify-separator "/"
                uniquify-ignore-buffers-re "^\\*"
                uniquify-after-kill-buffer-p t))


;;;
;;; hydra
;;;
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
  "
╭────────────────────────────────────────────────────────╯
  [_a_] Ace Window      [_v_] Split vertically
  [_t_] Transpose       [_x_] Split horizontally
  [_s_] Swap windows    [_o_] Delete other windows
  [_d_] Delete window   [_g_] Golden ratio
"
  ("a" ace-window :exit t)
  ("t" transpose-frame :exit t)
  ("o" ace-delete-other-windows :exit t)
  ("s" ace-swap-window :exit t)
  ("d" ace-delete-window :exit t)
  ("b" ivy-switch-buffer :exit t)
  ("g" golden-ratio :exit t)
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right)) "vert")
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)) "horz")
  ("q" nil))

(defhydra forge/hydra (:color blue)
  "
╭────────────────────────────────────────────────────────╯
  [_m_] Mail        [_s_] Eshell  [_p_] Packages
  [_w_] Windows     [_f_] Elfeed  [_g_] Magit
  [_n_] Navigation  [_j_] Jabber  [_S_] Slack
"
  ("w" forge/window/body)
  ("n" forge/navigate/body)
  ("m" notmuch)
  ("f" elfeed)
  ("j" forge/jabber-start-or-switch)
  ("g" magit-status)
  ("s" eshell-here)
  ("S" forge/slack/body)
  ("p" paradox-list-packages)
  ("q" nil))

(global-set-key (kbd "C-c n") 'forge/navigate/body)
(global-set-key (kbd "C-.") 'forge/hydra/body)


;;;
;;;
;;;
(provide 'forge-ui)
;;; forge-ui.el ends here
