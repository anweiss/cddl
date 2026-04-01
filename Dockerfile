FROM rust:slim AS builder
RUN apt-get update && apt-get install -y musl-tools musl-dev build-essential
ENV TARGET=x86_64-unknown-linux-musl
RUN rustup target add "$TARGET"
# Use musl-gcc (from musl-tools) as the linker for the musl target.
ENV CARGO_TARGET_X86_64_UNKNOWN_LINUX_MUSL_LINKER=musl-gcc
# Use thin LTO for Docker builds to reduce peak memory usage during linking.
# Full LTO (lto = true in Cargo.toml) can exceed the GitHub Actions runner
# memory limit when combined with resolver = "2" feature deduplication.
ENV CARGO_PROFILE_RELEASE_LTO=thin
WORKDIR /usr/src/cddl
COPY . .
RUN cargo build --release --bin cddl --target "$TARGET"

FROM scratch
COPY --from=builder /usr/src/cddl/target/x86_64-unknown-linux-musl/release/cddl /cddl
ENTRYPOINT [ "/cddl" ]