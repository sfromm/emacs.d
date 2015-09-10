(package-initialize)
(require 'package)
(require 'org)
(require 'ob-tangle)

; load up private configuration
(if (file-exists-p (concat user-emacs-directory "private.org"))
  (org-babel-load-file (concat user-emacs-directory "private.org"))
  )

; load up the bulk of my configuration
(org-babel-load-file (concat user-emacs-directory "emacs.org"))
