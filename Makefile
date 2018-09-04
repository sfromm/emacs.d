ARGS = -Q --batch --eval "(add-to-list 'load-path \"~/.emacs.d/site-lisp\")"
OS := $(shell uname)
VERSION = 26.1_1
ifeq ($(OS),Linux)
    EMACS = emacs $(ARGS)
endif
ifeq ($(OS),Darwin)
    EMACS = /usr/local/Cellar/emacs/$(VERSION)/Emacs.app/Contents/MacOS/Emacs $(ARGS)
endif

tangle:
	$(EMACS) --load build.el

update:
	git pull --rebase
	$(EMACS) --load site-lisp/setup-core.el -f "sf/bootstrap"
	$(EMACS) --load site-lisp/setup-core.el -f "sf/update-packages"

all: tangle packages

.PHONY: all tangle update packages
 
