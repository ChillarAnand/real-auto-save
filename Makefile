## Makefile

all:

REPO_USER    := ChillarAnand
PACKAGE_NAME := real-auto-save
REPO_NAME    := real-auto-save

EMACS        ?= emacs
ELS          := $(shell cask files)

##################################################

.PHONY: all

all: help

help:
	$(info )
	$(info Commands)
	$(info ========)
	$(info   - make          # Show this message)
	$(info   - make build    # Compile Elisp files)
	$(info   - make test     # Test $(PACKAGE_NAME))
	$(info )
	$(info Cleaning)
	$(info ========)
	$(info   - make clean    # Clean compiled files)
	$(info )
	$(info This Makefile required `cask`)
	$(info See https://github.com/$(REPO_USER)/$(REPO_NAME)#contribution)
	$(info )

##############################

%.elc: %.el .cask
	cask exec $(EMACS) --batch -f batch-byte-compile $<

.cask: Cask
	cask install
	touch $@

##############################

build: $(ELS:%.el=%.elc)

test: build
	cask exec $(EMACS) --batch -L . -l $(PACKAGE_NAME)-tests.el -f cort-test-run

clean:
	rm -rf $(ELS:%.el=%.elc) .cask
