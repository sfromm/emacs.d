(package-initialize)
(require 'package)
(require 'org)
(require 'ob-tangle)

; update load path to pull in tangled configs
(add-to-list 'load-path "~/.emacs.d/site-lisp")

; load up the bulk of my configuration
(require 'action-browser)
(org-babel-load-file (concat user-emacs-directory "emacs.org"))
