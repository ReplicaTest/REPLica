ARG IDRIS_VERSION=v0.5.1-7-ga9ccf4db
FROM snazzybucket/idris2:${IDRIS_VERSION} as builder

RUN mkdir /opt/replica
WORKDIR /opt/replica

COPY . ./

RUN make build
RUN pwd
RUN ls -R build

FROM ubuntu:20.04

RUN apt-get update && apt-get install --yes chezscheme && rm -rf /var/lib/apt/lists/*

RUN mkdir -p /opt/replica/exec

COPY --from=builder /opt/replica/build/exec /opt/replica/exec

ENV PATH="/opt/replica/exec:${PATH}"

ENTRYPOINT ["sh"]
