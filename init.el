(package-initialize)
(require 'package)
(require 'org)
(require 'ob-tangle)

; load up the bulk of my configuration
(org-babel-load-file (concat user-emacs-directory "emacs.org"))
