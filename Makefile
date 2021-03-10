.PHONY: build

build:
	idris2 --build replica.ipkg

install:
	idris2 --install replica.ipkg
	cp -r build/exec/* ${HOME}/.local/bin

clean:
	$(RM) -r build

test:
	build/exec/replica tests
