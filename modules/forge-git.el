;;; forge-git.el --- Set up the git -*- lexical-binding: t; -*-

;; Copyright (C) 2018 by Stephen Fromm

;;; Commentary:

;;; Code:

(use-package magit
  :ensure t
  :init
  (progn
    (setq
      magit-push-always-verify nil
      magit-completing-read-function 'ivy-completing-read
      magit-last-seen-setup-instructions "1.4.0")
    )
  :commands magit-status
  :bind ("C-x g" . magit-status))

(use-package magit-annex :ensure t)

(use-package git-annex :ensure t)

(use-package git-timemachine :ensure t)

(provide 'forge-git)
;;; forge-git.el ends here
