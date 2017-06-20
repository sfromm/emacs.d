EMACS = emacs --batch 

tangle: update
	$(EMACS) --load build.el

update:
	git pull --rebase

tanglenogit:
	$(EMACS) --load ~/.emacs.d/build.el

packages:
	$(EMACS) --load emacs.el -f "sf/update-packages"

all: tangle packages

.PHONY: all tangle update packages
