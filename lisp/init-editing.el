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

(defun forge/save-all ()
  "Save any file-related buffers."
  (interactive)
  (message "Saving buffers at %s" (format-time-string "%Y-%m-%dT%T"))
  (save-some-buffers t))

;; If focus switches away, save all files.
(when (version<= "24.4" emacs-version)
  (add-hook 'focus-out-hook 'forge/save-all))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)


(setopt version-control t)        ;; number each backup file
(setopt backup-by-copying t)      ;; instead of renaming current file
(setopt delete-old-versions t)    ;; clean up after oneself
(setopt kept-new-versions 5)      ;; Number of newer versions to keep.
(setopt kept-old-versions 5)      ;; Number of older versions to keep.
(setopt trash-directory "~/.Trash")
(setq backup-directory-alist (list (cons "." forge-backup-dir))
      tramp-backup-directory-alist backup-directory-alist)

;; Turn on auto-save, so we have a fallback in case of crashes or lost data.
;; Use `recover-file' or `recover-session' to recover them.
(setopt auto-save-default t)
(setopt auto-save-timeout 120)
(setopt auto-save-interval 64)
(setq auto-save-include-big-deletions t ;; don't auto-disable auto-save after deleting large chunks
      auto-save-list-file-prefix (expand-file-name "autosave/" forge-state-dir)
      ;; handle tramp paths differently than local ones, borrowed from doom
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))


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


;; display-line-numbers-mode
(when (fboundp 'display-line-numbers-mode)
  (let ((linum-hooks '(csv-mode-hook prog-mode-hook yaml-mode-hook yaml-ts-mode-hook)))
    (mapc (lambda (hook) (add-hook hook 'display-line-numbers-mode)) linum-hooks))
  (setopt display-line-numbers-width 3))


;; hl-line-mode
(let ((hl-line-hooks '(csv-mode-hook dired-mode-hook fle-mode-hook prog-mode-hook yaml-mode-hook yaml-ts-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))


(use-package highlight-indent-guides
  :custom (highlight-indent-guides-method 'character))

(defun my-whitespace-visualize ()
  "Enable whitespace visualizations."
  (setq highlight-tabs t)
  (setq show-trailing-whitespace t))

(let ((ws-visualize-hooks '(csv-mode-hook json-mode-hook prog-mode-hook yaml-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'my-whitespace-visualize)) ws-visualize-hooks))


(use-package hideshow
  :diminish hs-minor-mode
  :hook ((prog-mode) . hs-minor-mode)
  :bind (("C-c h" . hs-toggle-hiding)))


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

(use-package vlf-setup
  :ensure vlf
  :init (require 'vlf-setup))

(defun ffap-vlf ()
  "Find file at point with VLF."
  (interactive)
  (let ((file (ffap-file-at-point)))
    (unless (file-exists-p file)
      (error "File does not exist: %s" file))
    (vlf file)))


(use-package ediff
  :init
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))


(use-package diff-hl
  :commands (diff-hl-mode diff-hl-dired-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package envrc
  :bind (:map envrc-mode-map
              ("C-c e" . envrc-command-map))
  :hook (after-init . envrc-global-mode))


(provide 'init-editing)
