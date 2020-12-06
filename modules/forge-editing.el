;;; forge-editing.el --- This is an Emacs Lisp file with Emacs Lisp code.  -*- lexical-binding: t -*-

;; Copyright (C) 2018, 2019, 2020 Stephen Fromm

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

(show-paren-mode)
(setq-default indent-tabs-mode nil
	      require-final-newline t)


;;;
;;; page-break-lines
;;; More info:
;;; - http://endlessparentheses.com/improving-page-navigation.html
;;; - https://ericjmritz.wordpress.com/2015/08/29/using-page-breaks-in-gnu-emacs/
;;;
(use-package page-break-lines
  :ensure t
  :commands (turn-on-page-break-lines-mode)
  :diminish page-break-lines-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'turn-on-page-break-lines-mode))


;;;
;;; yasnippets
;;;
(use-package yasnippet
  :ensure t
  :diminish yasnippet-minor-mode
  :init
  (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (concat forge-personal-dir "snippets"))
  (add-hook 'term-mode-hook (lambda () "Disable yasnippet in terminal" (setq yas-dont-activate t))))



;;;
;;; expand-region
;;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :defer t
  :bind ("C-=" . er/expand-region))

;;;
;;; recentf
;;;
(use-package recentf
  :defer 2
  :bind ("<f7>" . ivy-recentf)
  :init
  (recentf-mode 1)
  :config
  (setq recentf-save-file (expand-file-name "recentf" forge-state-dir)
        recentf-max-menu-items 500
        recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "/tmp" "/ssh:")))


;;;
;;; with-editor
;;; https://github.com/magit/with-editor
(use-package with-editor   :defer t)

;;;
;;; uniquify
;;; Make buffer names legible
;;;
(use-package uniquify
  :defer t
  :init
  (setq
   uniquify-buffer-name-style 'forward
   uniquify-separator "/"
   uniquify-ignore-buffers-re "^\\*"
   uniquify-after-kill-buffer-p t))


;;;
;;; ediff
;;;
(use-package ediff
  :defer t
  :init
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))


;;;
;;; diff-hl
;;; https://github.com/dgutov/diff-hl
(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode diff-hl-dired-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))


;;;
;;; undo tree
;;;
(use-package undo-tree
  :defer t
  :diminish undo-tree-mode
  :bind
  (("C-/" . undo-tree-undo)
   ("C-?" . undo-tree-redo)
   ("C-x u" . undo-tree-visualize))
  :init
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))


;;;
;;; highlight-indent-guides
(use-package highlight-indent-guides
  :ensure t
  :custom (highlight-indent-guides-method 'character))

;;;
;;; Misc helpers
;;;
(defun forge/join-next-line ()
  "Join the next line with the current line."
  (interactive)
  (join-line -1))

(global-set-key (kbd "M-j") 'forge/join-next-line)

(defun forge/whitespace-visualize ()
  "Enable whitespace visualizations."
  (setq highlight-tabs t)
  (setq show-trailing-whitespace t))

;;;
;;; Fill paragraph ... unfill
;;; From http://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
;;;
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

;;;
;;; Backups and auto-save
;;;
(defun forge/save-all ()
  "Save any file-related buffers."
  (interactive)
  (message "Saving buffers at %s" (format-time-string "%Y-%m-%dT%T"))
  (save-some-buffers t))

(setq backup-directory-alist (list (cons ".*" forge-backup-dir)) ;; make backups of files to the backup directory
      auto-save-file-name-transforms `((".*" ,forge-backup-dir t))   ;;
      delete-old-versions -1
      version-control t
      savehist-file (concat forge-state-dir "savehist")
      savehist-save-minibuffer-history 1
      savehist-additional-variables '(kill-ring search-ring regexp-search-ring)
      auto-save-timeout 120
      auto-save-interval 1000)

(savehist-mode 1)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; If focus switches away, save all files.
(when (version<= "24.4" emacs-version)
  (add-hook 'focus-out-hook 'forge/save-all))


;;;
;;; https://www.emacswiki.org/emacs/DosToUnix
;;; Also consider using set-buffer-file-coding-system (C-x RET f) to "undecided-dos" or "undecided-unix"
(defun dos2unix (buffer)
  "Do replacement of ^M characters with newlines in BUFFER."
  ;; This is basically: "M-% C-q C-m RET C-q C-j RET"
  (interactive "*b")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (string ?\C-m) nil t)
      (replace-match (string ?\C-j) nil t))))


;;;
;;; flycheck
;;;
(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :custom (flycheck-global-modes '(not org-mode))
  :init (global-flycheck-mode))


(use-package company
  :defer t
  :hook (prog-mode . company-mode)
  :diminish company-mode)


;;;
;;; lisp
;;;
(use-package aggressive-indent
  :defer t
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package lisp-mode
  :defer t
  :hook
  (before-save . forge/turn-on-delete-trailing-whitespace)
  :config
  (setq lisp-indent-offset nil))

(use-package eldoc
  :defer t
  :diminish eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.3))



;;;
;;; python
;;;
(use-package python
  :defer t
  :interpreter ("python" . python-mode)
  :hook
  (python-mode . forge/turn-on-delete-trailing-whitespace)
  (python-mode . forge/whitespace-visualize)
  :config
  (setq-default python-indent-offset 4))

;;
;; https://github.com/proofit404/anaconda-mode
;;
(use-package anaconda-mode
  :defer t
  :after python
  :hook python-mode
  :init
  (setq anaconda-mode-installation-directory (expand-file-name "anaconda" forge-state-dir)))

(use-package company-anaconda
  :defer t
  :after anaconda-mode)


;;;
;;; shell scripts
;;;
(use-package shell-script
  :defer t
  :hook
  (shell-script . forge/whitespace-visualize)
  (shell-script . forge/turn-on-delete-trailing-whitespace))



;;;
;;; go-mode
;;;
(use-package go-mode
  :mode "\\.go\\ '"
  :defer t
  :config
  (add-hook 'before-save-hook #'gofmt-before-save))



;;;
;;; markdown mode
;;; https://github.com/jrblevin/markdown-mode
(use-package markdown-mode
  :defer t
  :custom
  (markdown-command "pandoc -f markdown_github+smart")
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (defun orgtbl-to-gfm (table params)
    "Convert the Orgtbl mode TABLE to GitHub Flavored Markdown."
    (let* ((alignment (mapconcat (lambda (x) (if x "|--:" "|---"))
                                 org-table-last-alignment ""))
           (params2
            (list
             :splice t
             :hline (concat alignment "|")
             :lstart "| " :lend " |" :sep " | ")))
      (orgtbl-to-generic table (org-combine-plists params2 params)))))



;;;
;;; web mode
;;; For html, css, and related files.
(use-package web-mode
  :defer t
  :init
  (progn
    (setq
     web-mode-css-indent-offset 2
     web-mode-markup-indent-offset 2
     web-mode-code-indent-offset 2)
    (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))))


(use-package php-mode
  :defer t)


;;;
;;; yaml mode
;;;
(use-package yaml-mode
  :defer t
  :hook
  (yaml-mode . forge/turn-on-delete-trailing-whitespace)
  (yaml-mode . forge/whitespace-visualize)
  :config
  (setq yaml-indent-offset 2))



;;;
;;; json mode
;;;
(use-package json-mode
  :defer t
  :hook
  (json-mode . forge/turn-on-delete-trailing-whitespace)
  (json-mode . forge/whitespace-visualize))



;;;
;;; ledger-mode
;;; For editing ledger files.
(use-package ledger-mode
  :defer t)


;;;
;;; junos-mode
;;;
(use-package junos-mode
  :defer t
  :config (setq-local c-basic-offset 4))


;;;
;;; eos-mode
(use-package eos-mode
  :defer t
  :quelpa (eos-mode :fetcher github :repo "sfromm/eos-mode")
  :hook (eos-mode . highlight-indent-guides-mode))


;;;
;;;
;;;
(provide 'forge-editing)
;;; forge-editing.el ends here
