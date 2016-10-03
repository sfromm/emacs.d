;;; build - Tangle all org-babel files to their configy-goodness
;;;
;;; The goal is that this can be pulled in by emacs from the CLI
;;; tangle out all the relevant dot-files.  This then can be called
;;; by make or something similar.
;;;
;;; Invocation:  emacs --batch --load ~/.emacs.d/build.el

(require 'org)       ;; org-mode
(require 'ob)        ;; org-mode export
(require 'ob-tangle) ;; and finally org-mode tangle

(defun sf/tangle-file (file)
  "Given an 'org-mode' FILE, tangle the source code"
  (interactive "fOrg File: ")
  (find-file file)
  (org-babel-tangle)
  (kill-buffer))

(defun sf/tangle-dot-emacs ()
  "Tangle Emacs configuration from org-babel"
  (sf/tangle-file "~/.emacs.d/emacs.org"))

(defun sf/tangle-dot-files ()
  "Tangle dot files from org-babel"
  (sf/tangle-file "~/depot/doc/org/documentation.org"))

(defun sf/build-configs ()
  "Tangle all configs"
  (interactive)
  (sf/tangle-dot-emacs)
  (sf/tangle-dot-files))

(sf/build-configs)

(provide 'dot-files)
