.PHONY: build all

.SUFFIXES: .dhall .json

TEST=tests.json
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
	cp -r build/exec/* ${DEST}

clean-test:
	${RM} ${TEST}

clean: clean-test
	${RM} -r build

.dhall.json:
	dhall-to-json --file $? --output $@

generate: clean-test ${TEST} build
	build/exec/replica ${GLOBAL} run ${RUN} --interactive ${TEST}

test: clean-test ${TEST} build
	build/exec/replica ${GLOBAL} run ${RUN} ${TEST}

docker-build:
	docker build . -t berewt/replica

docker-run:
	docker run berewt/replica

all: test install docker-build
