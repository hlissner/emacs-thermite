EMACS := emacs
BUTTERCUPLIB := ./.buttercup

all: test

test:
	@[[ -d "$(BUTTERCUPLIB)" ]] || git clone https://github.com/jorgenschaefer/emacs-buttercup "$(BUTTERCUPLIB)"
	@$(EMACS) -batch -L . -L ./.buttercup \
		-l ./test/helpers.el \
		-l buttercup \
		-f buttercup-run-discover

%.elc: %.el
	$(EMACS) -batch -L . -f batch-byte-compile $<


.PHONY: test
