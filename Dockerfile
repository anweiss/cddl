FROM rust:1.64.0-alpine AS builder
RUN apk update
RUN apk add --no-cache openssl-dev musl-dev
WORKDIR /usr/src/cddl
COPY . ./
RUN cargo b --release --bin cddl

FROM alpine:latest
COPY --from=builder /usr/src/cddl/target/release/cddl /usr/local/bin/cddl
ENTRYPOINT [ "/usr/local/bin/cddl" ]