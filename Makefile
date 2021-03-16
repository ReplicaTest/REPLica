.PHONY: build

testsName = tests

build:
	idris2 --build replica.ipkg

install:
	idris2 --install replica.ipkg
	cp -r build/exec/* ${HOME}/.local/bin

clean:
	$(RM) -r build

showtests:
	dhall-to-json --file ${testsName}.dhall

generate:
	dhall-to-json --file ${testsName}.dhall --output ${testsName}.json
	build/exec/replica run --interactive ${testsName}.json
	rm ${testsName}.json

test:
	dhall-to-json --file ${testsName}.dhall --output ${testsName}.json
	build/exec/replica run ${testsName}.json
	rm ${testsName}.json
