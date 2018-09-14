;;; init.el -- Forge configuration initialization

;; Copyright (C) 2018 by Stephen Fromm

;;; Commentary:
;; Load up the forge modules.

;;; Code:

(message "Loading up Emacs...")

(require 'forge-core (concat user-emacs-directory "modules/forge-core"))

(forge/load-modules 'forge-appearance
                    'forge-editing
                    'forge-ui
                    'forge-dired
                    'forge-git
                    'forge-eshell
                    'forge-dired
                    'forge-markdown
                    'forge-orgmode
                    'forge-pass
                    'forge-web)

(message "Emacs is ready, finished loading after %.03fs."
  (float-time (time-subtract after-init-time before-init-time)))

;;; init.el ends here
