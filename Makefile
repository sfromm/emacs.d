EMACS = emacs -Q --batch --eval "(add-to-list 'load-path \"~/.emacs.d/site-lisp\")"

tangle:
	$(EMACS) --load build.el

update:
	git pull --rebase
	$(EMACS) --load site-lisp/setup-core.el -f "sf/bootstrap"
	$(EMACS) --load site-lisp/setup-core.el -f "sf/update-packages"

tanglenogit:
	$(EMACS) --load ~/.emacs.d/build.el


all: tangle packages

.PHONY: all tangle update packages
