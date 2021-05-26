.PHONY: build all

.SUFFIXES: .dhall .json

TEST=tests.json
DEST=${HOME}/.local/bin

build:
	idris2 --build replica.ipkg

install: build
	mkdir -p ${DEST}
	cp -r build/exec/* ${DEST}

clean:
	${RM} -r build
	${RM} ${TEST}

.dhall.json:
	dhall-to-json --file $? --output $@

generate: ${TEST}
	build/exec/replica ${GLOBAL} run ${RUN} --interactive ${TEST}

test: ${TEST} build
	build/exec/replica ${GLOBAL} run ${RUN} ${TEST}

docker-build:
	docker build . -t berewt/replica

docker-run:
	docker run berewt/replica

all: test install docker-build
