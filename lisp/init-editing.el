;;; init-editing.el --- Init general editing support -*- lexical-binding: t -*-
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


(show-paren-mode)
(add-hook 'text-mode-hook 'visual-line-mode)
(setq-default indent-tabs-mode nil
              fill-column 80
              require-final-newline t)

(defun forge/join-next-line ()
  "Join the next line with the current line."
  (interactive)
  (join-line -1))

(global-set-key (kbd "M-j") 'forge/join-next-line)

(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph] #'endless/fill-or-unfill)

(when (fboundp 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3))


(use-package page-break-lines
  :diminish page-break-lines-mode
  :hook
  (emacs-lisp-mode . page-break-lines-mode))


(use-package yasnippet
  :diminish yasnippet-minor-mode
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" forge-personal-dir))
  (add-hook 'term-mode-hook (lambda () "Disable yasnippet in terminal" (setq yas-dont-activate t))))


(use-package expand-region
  :bind ("C-=" . er/expand-region))


(use-package hideshow
  :diminish hs-minor-mode
  :hook ((prog-mode) . hs-minor-mode)
  :bind (("C-c h" . hs-toggle-hiding)))


(use-package highlight-indent-guides
  :custom (highlight-indent-guides-method 'character))


(use-package recentf
  :bind ("<f7>" . consult-recent-file)
  :custom
  (recentf-save-file (expand-file-name "recentf" forge-state-dir))
  (recentf-max-menu-items 500)
  (recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "/tmp" "/ssh:"))
  :init
  (recentf-mode 1))


(defun dos2unix (buffer)
  "Do replacement of ^M characters with newlines in BUFFER."
  ;; This is basically: "M-% C-q C-m RET C-q C-j RET"
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t))))


(use-package ediff
  :init
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))


(use-package diff-hl
  :commands (diff-hl-mode diff-hl-dired-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))


(use-package corfu
  :custom
  (corfu-separator ?\s)
  :init
  (global-corfu-mode))


(provide 'init-editing)
