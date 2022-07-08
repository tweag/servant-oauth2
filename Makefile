.PHONY: noop

noop:
	@echo Noop!

format:
	fourmolu --mode inplace $$(git ls-files '*.hs')

