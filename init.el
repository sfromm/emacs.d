;;; init.el -- Forge configuration initialization

;; Copyright (C) 2018 by Stephen Fromm

;;; Commentary:
;; Load up the forge modules.

;;; Code:

(message "Loading up Emacs...")

(require 'forge-core (concat user-emacs-directory "modules/forge-core"))

(require 'forge-appearance)
(require 'forge-ui)
(require 'forge-dired)
(require 'forge-git)
(require 'forge-pass)
(require 'forge-eshell)
(require 'forge-editing)
(require 'forge-orgmode)

(message "Emacs is ready, finished loading after %.03fs."
  (float-time (time-subtract after-init-time before-init-time)))

;;; init.el ends here
