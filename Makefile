EMACS_ARGS = --quick --batch --load init.el
OS := $(shell uname)
ARCH := $(shell uname -m)

ifeq ($(OS),Linux)
    EMACS = emacs
    PKGMGR = apt-get install
    PKG = emacs
    PKGFLAGS =
    FONTS = fonts-hack-ttf fonts-firacode fonts-ibm-plex
endif
ifeq ($(OS),Darwin)
    EMACS = emacs
    PKGMGR = brew install
    PKG = emacs-plus@30
    PKGFLAGS = --with-native-comp --with-imagemagick --with-no-frame-refocus --with-savchenkovaleriy-big-sur-icon
    FONTS = font-hack font-fira-code font-fira-mono font-fira-sans font-victor-mono font-jetbrains-mono font-sf-pro font-sf-mono font-ibm-plex-sans font-ibm-plex-mono
    ifeq ($(ARCH),arm64)
	BREW_PATH = /opt/homebrew/bin
    else
	BREW_PATH = /usr/local/bin
    endif
endif

ORG_ARGS  = --batch -Q
ORG_EVAL += --eval "(setq indent-tabs-mode nil)"
ORG_EVAL += --eval "(setq org-src-preserve-indentation nil)"
ORG_EVAL += --eval "(setq org-html-checkbox-type 'html)"
ORG_EVAL += --eval "(require 'ox-html)"
ORG_EVAL += --funcall org-html-export-to-html

install: tap
	$(PKGMGR) $(PKG) $(PKGFLAGS)

tap:
	brew tap d12frosted/$(PKG)

bootstrap:
	@echo "bootstrapping packages we depend on"
	$(EMACS) $(EMACS_ARGS) -f "init-install-core-packages"
	$(EMACS) $(EMACS_ARGS) -f "my-package-install"

update:
	git diff-files --quiet && git pull --rebase

upgrade: update
	@echo "upgrading existing emacs pacakges"
	$(EMACS) $(EMACS_ARGS) -f "my-package-upgrade-packages"

html:
	@echo "exporting to html"
	$(EMACS) $(ORG_ARGS) emacs.org $(ORG_EVAL)

fonts:
	test -x $(BREW_PATH)/brew && brew install --cask $(FONTS) || $(PKGMGR) $(FONTS)

all: fonts install bootstrap

.PHONY: all install tap bootstrap update upgrade fonts
