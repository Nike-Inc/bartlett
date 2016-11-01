
build: clean
	@stack build

watch: build
	@stack build --test --coverage --haddock --copy-bins --file-watch

static-bin: build
	@stack build --ghc-options='-optl-static -optl-pthread' \
		--force-dirty --copy-bins

package-bin: static-bin
	@tar czvf bartlett.tar.gz \
		-C "${HOME}/.local/bin/" bartlett

clean:
	@stack clean
