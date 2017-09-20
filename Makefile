EMACS = emacs -Q --batch --eval "(add-to-list 'load-path \"~/.emacs.d/site-lisp\")"

tangle:
	$(EMACS) --load build.el

update:
	$(EMACS) --load site-lisp/setup-core.el -f "sf/bootstrap"
	$(EMACS) --load site-lisp/setup-core.el -f "sf/update-packages"
	git pull --rebase

tanglenogit:
	$(EMACS) --load ~/.emacs.d/build.el


all: tangle packages

.PHONY: all tangle update packages
