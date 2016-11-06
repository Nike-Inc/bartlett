
build: clean
	@stack build

watch: build
	@stack build --test --coverage --haddock --copy-bins --file-watch

package-bin: build
	@tar czvf bartlett.tar.gz \
		-C "${HOME}/.local/bin/" bartlett

clean:
	@stack clean
