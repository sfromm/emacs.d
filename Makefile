ARGS = --quick --batch --load init.el
OS := $(shell uname)
ARCH := $(shell uname -m)

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
    PKG = emacs-plus@29
    PKGFLAGS = --with-native-comp
    FONTS = font-hack font-fira-code font-fira-mono font-fira-sans font-victor-mono font-jetbrains-mono
    ifeq ($(ARCH),arm64)
	BREW_PATH = /opt/homebrew/bin
    else
	BREW_PATH = /usr/local/bin
    endif
endif

install: tap
	$(PKGMGR) $(PKG) $(PKGFLAGS)

tap:
	brew tap d12frosted/$(PKG)

bootstrap:
	@echo "bootstrapping packages we depend on"
	$(EMACS) -f "init-install-core-packages"
	$(EMACS) -f "my-package-install"

update:
	git diff-files --quiet && git pull --rebase

upgrade: update
	@echo "upgrading existing emacs pacakges"
	$(EMACS) -f "my-package-upgrade-packages"

fonts:
	test -x $(BREW_PATH)/brew && brew tap homebrew/cask-fonts || true
	test -x $(BREW_PATH)/brew && brew cask install $(FONTS) || $(PKGMGR) $(FONTS)

all: fonts install bootstrap

.PHONY: all install tap bootstrap update upgrade fonts
