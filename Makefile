EMACS = emacs --batch 

all: tangle update

tangle:
	$(EMACS) --load build.el

update: tangle
	$(EMACS) --load emacs.el -f "sf/update-packages"

.PHONY: all
