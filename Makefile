ARGS = --quick --batch --load modules/forge-core.el
OS := $(shell uname)
VERSION = 26.1_1
ifeq ($(OS),Linux)
    EMACS = emacs $(ARGS)
    PKGMGR = apt-get install
    PKGFLAGS =
    FONTS = fonts-hack-ttf fonts-firacode
endif
ifeq ($(OS),Darwin)
    EMACS = emacs $(ARGS)
    PKGMGR = brew cask install
    PKGFLAGS =
    FONTS = font-hack font-fira-code font-fira-mono font-fira-sans font-symbola
endif

install: fonts
	$(PKGMGR) emacs $(PKGFLAGS)

bootstrap:
	$(EMACS) -f "forge/bootstrap-packages"

update:
	git pull --rebase

upgrade: update
	$(EMACS) -f "forge/upgrade-packages"

fonts:
	test -x /usr/local/bin/brew && brew tap homebrew/cask-fonts || true
	$(PKGMGR) $(FONTS)

all: install bootstrap

.PHONY: all install bootstrap update fonts
