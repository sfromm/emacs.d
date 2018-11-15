ARGS = --quick --batch --load modules/forge-core.el
OS := $(shell uname)
VERSION = 26.1_1
ifeq ($(OS),Linux)
    EMACS = emacs $(ARGS)
    PKGMGR = apt-get install
    PKGFLAGS =
endif
ifeq ($(OS),Darwin)
    EMACS = emacs $(ARGS)
    PKGMGR = brew install
    PKGFLAGS = --with-cocoa --with-librsvg --with-gnutls --with-modules --with-imagemagick@6
endif

install:
	$(PKGMGR) emacs $(PKGFLAGS)

bootstrap:
	$(EMACS) -f "forge/bootstrap-packages"

update:
	git pull --rebase

upgrade:
	$(EMACS) -f "forge/upgrade-packages"

all: tangle packages

.PHONY: all install update
