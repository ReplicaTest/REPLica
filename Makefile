.PHONY: build

testsName = tests

build:
	idris2 --build replica.ipkg

install:
	idris2 --install replica.ipkg
	cp -r build/exec/* ${HOME}/.local/bin

clean:
	$(RM) -r build
	$(RM) ${testsName}.json

showtests:
	dhall-to-json --file ${testsName}.dhall

${testsName}.json:
	dhall-to-json --file ${testsName}.dhall --output ${testsName}.json

generate: ${testsName}.json
	build/exec/replica run --interactive ${testsName}.json

test: ${testsName}.json build
	build/exec/replica run ${testsName}.json
