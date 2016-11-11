
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
