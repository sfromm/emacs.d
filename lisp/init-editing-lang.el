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

(defun my-treesitter-setup ()
  "Set up treesitter for use in environment."
  (interactive)
  (when (treesit-available-p)
    (setq treesit-language-source-alist
          '((bash "https://github.com/tree-sitter/tree-sitter-bash")
            ;; (cmake "https://github.com/uyha/tree-sitter-cmake")
            ;; (css "https://github.com/tree-sitter/tree-sitter-css")
            (elisp "https://github.com/Wilfred/tree-sitter-elisp")
            ;; (go "https://github.com/tree-sitter/tree-sitter-go")
            ;; (html "https://github.com/tree-sitter/tree-sitter-html")
            ;; (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
            (json "https://github.com/tree-sitter/tree-sitter-json")
            (make "https://github.com/alemuller/tree-sitter-make")
            (markdown "https://github.com/ikatyang/tree-sitter-markdown")
            (python "https://github.com/tree-sitter/tree-sitter-python")
            ;; (toml "https://github.com/tree-sitter/tree-sitter-toml")
            ;; (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
            ;; (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
            (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
    (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))))

(use-package emacs
  :config
  (setq major-mode-remap-alist
        '((bash-mode . bash-ts-mode)
          (json-mode . json-ts-mode)
          ;; (markdown-mode . markdown-ts-mode)
          (python-mode . python-ts-mode)
          (yaml-mode . yaml-ts-mode)))
  :hook
  ((prog-mode . electric-pair-mode)))

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-auto-shudown t)
  (eglot-extend-to-xref t)
  :config
  ;; (setq-default eglot-workspace-configuration
  ;;               '((:pylsp . (:configurationSources ["flake8"]
  ;;                                                  :plugins (
  ;;                                                            :pycodestyle (:enabled nil)
  ;;                                                            :mccabe (:enabled nil)
  ;;                                                            :flake8 (:enabled t))))))
  ;; (add-to-list 'eglot-server-programs
  ;;              '(python-mode . ("pylsp"))
  ;;              '(yaml-mode . ("yaml-language-server")))
  (fset #'jsonrpc--log-event #'ignore)  ; don't log every event
  :hook
  (python-mode . eglot-ensure)
  (yaml-mode . eglot-ensure))


(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package docker-compose-mode
  :mode "docker-compose.*\.yml\\'")


(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(with-eval-after-load 'lisp-mode
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


(use-package go-mode
  :mode "\\.go\\'"
  :config
  (add-hook 'before-save-hook #'gofmt-before-save))


(use-package ess)


(use-package web-mode
  :mode ("\\.html\\'" "\\.j2\\'")
  :init
  (setq web-mode-enable-auto-indentation nil ;; temporary for now.
        web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(use-package graphql-mode
  :mode ("\\.graphql\\'" . graphql-mode))

(use-package php-mode
  :mode "\\.php\\'")


(use-package csv-mode)

(use-package json-mode)

(use-package yaml-mode
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
  (add-to-list 'magic-mode-alist '("!RANCID-CONTENT-TYPE: juniper" . junos-mode))
  (add-to-list 'magic-mode-alist '("!RANCID-CONTENT-TYPE: junos" . junos-mode))
  (setq-local c-basic-offset 4))

(use-package eos-mode
  :ensure nil ;; install via package-vc-install
  :init (init-vc-install :fetcher "github" :repo "sfromm/eos-mode")
  :commands (eos-mode)
  :magic ("!RANCID-CONTENT-TYPE: arista" . eos-mode)
  :hook (eos-mode . highlight-indent-guides-mode))

(use-package yang-mode)

(use-package nftables-mode)


(use-package fle-mode
  :ensure nil
  :init (init-vc-install :fetcher "github" :repo "sfromm/fle-mode")
  :commands (fle-mode)
  :config
  (setq-local tab-width 8))


(use-package ledger-mode
  :commands ledger-mode)

(use-package beancount
  :ensure nil
  :init (init-vc-install :fetcher "github" :repo "beancount/beancount-mode")
  :mode ("\\.beancount\\'" . beancount-mode)
  :hook (beancount-mode . outline-minor-mode))


(provide 'init-editing-lang)
