((magit-blame
  ("-w"))
 (magit-branch nil)
 (magit-commit nil)
 (magit-dispatch nil)
 (magit-log
  ("-n256" "--graph" "--decorate")
  ("-n256" "--follow" "--graph" "--decorate")
  ("-n256"
   ("--" "init-notmuch.el")
   "--graph" "--decorate"))
 (magit-pull nil)
 (magit-push nil)
 (magit-stash nil)
 (magit:-- "init-notmuch.el" ""))
