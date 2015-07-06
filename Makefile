.PHONY: test update cask cask-update clean

update: cask cask-update

cask:
	cask
	EMACS=emacs-24.1 cask
	EMACS=emacs-24.2 cask
	EMACS=emacs-24.3 cask
	EMACS=emacs-24.4 cask
	EMACS=emacs-24.5 cask

cask-update:
	cask update
	EMACS=emacs-24.1 cask update
	EMACS=emacs-24.2 cask update
	EMACS=emacs-24.3 cask update
	EMACS=emacs-24.4 cask update
	EMACS=emacs-24.5 cask update

test:
	cask exec ert-runner --quiet

elisp-test-all:
	cask exec ert-runner --quiet
	EMACS=emacs-24.1 cask exec ert-runner --quiet
	EMACS=emacs-24.2 cask exec ert-runner --quiet
	EMACS=emacs-24.3 cask exec ert-runner --quiet
	EMACS=emacs-24.4 cask exec ert-runner --quiet
	EMACS=emacs-24.5 cask exec ert-runner --quiet

clean:
	find ./* -name '*.elc' -delete
