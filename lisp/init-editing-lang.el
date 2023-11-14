;;; init-editing-lang.el --- Init support for various languages -*- lexical-binding: t -*-
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


(use-package flycheck
  :diminish flycheck-mode
  :custom (flycheck-global-modes '(not org-mode))
  :init (global-flycheck-mode))


(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package docker-compose-mode
  :mode "docker-compose.*\.yml\\'")


(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(with-eval-after-load 'lisp-mode
  (add-hook 'before-save-hook #'forge/turn-on-delete-trailing-whitespace)
    (setq lisp-indent-offset nil))

(use-package eldoc
  :diminish eldoc-mode
  :hook
  (emacs-lisp-mode . eldoc-mode)
  (lisp-interaction-mode . eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.3))


(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-command "pandoc -f markdown_github+smart")
  :preface
  (defun orgtbl-to-gfm (table params)
    "Convert the Orgtbl mode TABLE to GitHub Flavored Markdown."
    (let* ((alignment (mapconcat (lambda (x) (if x "|--:" "|---"))
                                 org-table-last-alignment ""))
           (params2
            (list
             :splice t
             :hline (concat alignment "|")
             :lstart "| " :lend " |" :sep " | ")))
      (orgtbl-to-generic table (org-combine-plists params2 params))))

  (defun forge/insert-org-to-md-table (table-name)
    "Helper function to create markdown and orgtbl boilerplate."
    (interactive "*sEnter table name: ")
    (insert "<!---
#+ORGTBL: SEND " table-name " orgtbl-to-gfm

-->
<!--- BEGIN RECEIVE ORGTBL " table-name " -->
<!--- END RECEIVE ORGTBL " table-name " -->")
    (previous-line)
    (previous-line)
    (previous-line)))


(use-package python
  :interpreter ("python" . python-mode)
  :hook
  (python-mode . forge/turn-on-delete-trailing-whitespace)
  (python-mode . forge/whitespace-visualize)
  :config
  ;; set python-shell-interpreter to python3
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))
  (setq-default python-indent-offset 4))

(use-package anaconda-mode
  :after python
  :hook python-mode
  :init
  (setq anaconda-mode-installation-directory (expand-file-name "anaconda" forge-state-dir)))

(use-package company-anaconda
  :after anaconda-mode)


(use-package go-mode
  :mode "\\.go\\ '"
  :config
  (add-hook 'before-save-hook #'gofmt-before-save))

(use-package ess)

(with-eval-after-load 'shell-script-mode
  (add-hook 'shell-script-hook #'forge/whitespace-visualize)
  (add-hook 'shell-script-hook #'forge/turn-on-delete-trailing-whitespace))


(use-package web-mode
  :mode ("\\.html\\'" "\\.j2\\'")
  :init
  (setq web-mode-enable-auto-indentation nil ;; temporary for now.
        web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package graphql-mode
  :mode ("\\.graphql\\'" . graphql-mode))

(use-package php-mode
  :mode "\\.php\\'")


(use-package csv-mode
  :hook
  (csv-mode . forge/toggle-highlight-line)
  (csv-mode . forge/turn-on-delete-trailing-whitespace)
  (csv-mode . forge/whitespace-visualize))

(use-package json-mode
  :hook
  (json-mode . forge/turn-on-delete-trailing-whitespace)
  (json-mode . forge/whitespace-visualize))

(use-package yaml-mode
  :hook
  (yaml-mode . forge/turn-on-delete-trailing-whitespace)
  (yaml-mode . forge/whitespace-visualize)
  :config
  (setq yaml-indent-offset 2))

(with-eval-after-load 'nxml
  (defalias 'xml-mode 'nxml-mode)
  (autoload 'sgml-skip-tag-forward "sgml-mode")
  (add-to-list 'hs-special-modes-alist
               '(nxml-mode
                 "<!--\\|<[^/>]*[^/]>"
                 "-->\\|</[^/>]*[^/]>"
                 "<!--"
                 sgml-skip-tag-forward
                 nil)))


(with-eval-after-load 'junos-mode
  (add-to-list 'magic-mode-alist '("!RANCID-CONTENT-TYPE: fujitsu_1finity" . junos-mode))
  (setq-local c-basic-offset 4))

(use-package eos-mode
  :quelpa (eos-mode :fetcher github :repo "sfromm/eos-mode")
  :commands (eos-mode)
  :magic ("!RANCID-CONTENT-TYPE: arista" . eos-mode)
  :hook (eos-mode . highlight-indent-guides-mode))

(use-package yang-mode)

(use-package nftables-mode)

(use-package fle-mode
  :quelpa (fle-mode :fetcher github :repo "sfromm/fle-mode")
  :commands (fle-mode))

(use-package ledger-mode
  :commands ledger-mode)

(provide 'init-editing-lang)
