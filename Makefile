.PHONY: build all FORCE clean clean-test generate tests docker-run docker-build

.SUFFIXES: .dhall .json

TEST=tests.json

REPLICA_TESTS= $(wildcard ./tests/replica/*/*.json)

DEST=${HOME}/.local/bin

FORCE:

src/Replica/Version.idr: FORCE # We force the update of the version on build
	echo "module Replica.Version" > src/Replica/Version.idr
	echo "" >> src/Replica/Version.idr
	echo "export" >> src/Replica/Version.idr
	echo "version : String" >> src/Replica/Version.idr
	echo "version = \"`git describe --tags`\"" >> src/Replica/Version.idr

build: src/Replica/Version.idr
	idris2 --build replica.ipkg

install: build
	mkdir -p ${DEST}
	${RM} ${DEST}/replica
	cp -r build/exec/* ${DEST}

clean-test:
	${RM} ${TEST}
	${RM} ${REPLICA_TESTS}

clean: clean-test
	${RM} -r build

.dhall.json:
	dhall-to-json --file $? --output $@

generate: ${REPLICA_TESTS} ${TEST} build
	build/exec/replica ${GLOBAL} run ${RUN} --interactive ${TEST}

test: ${REPLICA_TESTS} ${TEST} build
	build/exec/replica ${GLOBAL} run ${RUN} ${TEST}

docker-build:
	docker build . -t berewt/replica

docker-run:
	docker run berewt/replica

all: test install docker-build
