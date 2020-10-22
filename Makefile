ARGS = --quick --batch --load modules/forge-core.el
OS := $(shell uname)
VERSION = 26.1_1
ifeq ($(OS),Linux)
    EMACS = emacs $(ARGS)
    PKGMGR = apt-get install
    PKG = emacs
    PKGFLAGS =
    FONTS = fonts-hack-ttf fonts-firacode fonts-ibm-plex
endif
ifeq ($(OS),Darwin)
    EMACS = emacs $(ARGS)
    PKGMGR = brew install
    PKG = emacs-plus
    PKGFLAGS =
    FONTS = font-hack font-fira-code font-fira-mono font-fira-sans font-victor-mono
endif

install: tap
	$(PKGMGR) $(PKG) $(PKGFLAGS)

tap:
	brew tap d12frosted/$(PKG)

bootstrap:
	$(EMACS) -f "forge/bootstrap-packages"

update:
	git diff-files --quiet && git pull --rebase

upgrade: update
	$(EMACS) -f "forge/upgrade-packages"

fonts:
	test -x /usr/local/bin/brew && brew tap homebrew/cask-fonts || true
	test -x /usr/local/bin/brew && brew cask install $(FONTS) || $(PKGMGR) $(FONTS)

all: fonts install bootstrap

.PHONY: all install tap bootstrap update upgrade fonts
