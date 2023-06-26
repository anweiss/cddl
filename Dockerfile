FROM rust:slim AS builder
RUN apt update && apt install -y musl-tools musl-dev
RUN apt-get install -y build-essential
RUN yes | apt install gcc-x86-64-linux-gnu
ENV TARGET x86_64-unknown-linux-musl
RUN rustup target add "$TARGET"
ENV RUSTFLAGS='-C linker=x86_64-linux-gnu-gcc'
COPY . ./
RUN cargo b --release --bin cddl --target "$TARGET"

FROM scratch
COPY --from=builder /target/x86_64-unknown-linux-musl/release/cddl /cddl
ENTRYPOINT [ "/cddl" ]