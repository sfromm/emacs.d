ARGS = -Q --batch --eval "(add-to-list 'load-path \"~/.emacs.d/site-lisp\")"
OS := $(shell uname)
VERSION = 26.1_1
ifeq ($(OS),Linux)
    EMACS = emacs $(ARGS)
    PKGMGR = apt-get install
    PKGFLAGS =
endif
ifeq ($(OS),Darwin)
    EMACS = emacs $(ARGS)
    PKGMGR = brew
    PKGFLAGS = --with-cocoa --with-librsvg --with-modules --with-gnutls
endif

install:
	$(PKGMGR) emacs $(PKGFLAGS)

bootstrap:
	$(EMACS) --load modules/forge-core.el -f "forge/bootstrap-packages"

update:
#	git pull --rebase
	$(EMACS) --load modules/forge-core.el -f "forge/update-packages"

all: tangle packages

.PHONY: all install update
