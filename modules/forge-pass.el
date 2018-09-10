;;; forge-pass.el --- Set up the pass -*- lexical-binding: t; -*-

;; Copyright (C) 2018 by Stephen Fromm

;;; Commentary:

;;; Code:

(when (forge/system-type-is-darwin)
  (custom-set-variables '(epg-gpg-program "/usr/local/bin/gpg"))
  (setq epa-pinentry-mode 'loopback))

(use-package auth-source)

(use-package pass
  :ensure t
  :bind
  ("C-c p" . pass))

(use-package auth-password-store
  :after auth-source
  :init
  (setq auth-sources '(password-store "~/.authinfo.gpg")))

(provide 'forge-pass)
;;; forge-pass.el ends here
