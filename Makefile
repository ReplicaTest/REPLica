.PHONY: build all

.SUFFIXES: .dhall .json

TEST=tests.json
REPLICA_GLOBAL=""
REPLICA_RUN_OPTIONS=""
DEST=${HOME}/.local/bin

build: build/exec/replica
	idris2 --build replica.ipkg

install:
	idris2 --install replica.ipkg
	mkdir -p ${DEST}
	cp -r build/exec/* ${DEST}

clean:
	${RM} -r build
	${RM} ${TEST}

.dhall.json:
	dhall-to-json --file $? --output $@

generate: ${TEST}
	build/exec/replica ${REPLICA_GLOBAL} run ${REPLICA_RUN_OPTIONS} --interactive ${TEST}

test: ${TEST} build
	build/exec/replica run ${TEST}

all: build test
