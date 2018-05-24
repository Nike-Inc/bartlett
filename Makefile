.PHONY: hlint_install, hlint, hlint_apply_refact, hlint_refactor
.PHONY: stylish_haskell_install, stylish_haskell_check, clean

build: clean
	@stack install

watch: build
	@stack build --test --coverage --haddock --copy-bins --file-watch

static-build: clean
	@stack install && strip "${HOME}/.local/bin/bartlett"

package-bin: static-build
	@tar czvf bartlett.tar.gz \
		-C "${HOME}/.local/bin/" bartlett

clean:
	@stack clean

# The following tasks cribbed from: https://lwm.github.io/haskell-static/
hlint_install:
	@stack install hlint

hlint: hlint_install
	@hlint test/ src/ app/

hlint_apply_refact: hlint_install
	@stack install apply-refact

HLINT=hlint --refactor --refactor-options -i {} \;
hlint_refactor: hlint_apply_refact
	@find src/ test/ app/ -name "*.hs" -exec $(HLINT)

stylish_haskell_install:
	@stack install stylish-haskell

STYLISH=stylish-haskell -i {} \;
stylish_haskell_check: stylish_haskell_install
	@find test/ app/ src/ -name "*.hs" -exec $(STYLISH) && git diff --exit-code
