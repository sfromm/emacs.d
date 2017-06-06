EMACS = emacs --batch 

all: tangle packages

tangle: update
	$(EMACS) --load build.el

update:
	git pull --rebase

packages:
	$(EMACS) --load emacs.el -f "sf/update-packages"

.PHONY: all tangle update packages
