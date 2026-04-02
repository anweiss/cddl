FROM rust:slim AS builder
ARG TARGETARCH
RUN apt-get update && apt-get install -y musl-tools musl-dev build-essential
# Set Rust target based on Docker build platform
RUN if [ "$TARGETARCH" = "arm64" ]; then \
      echo "aarch64-unknown-linux-musl" > /tmp/rust-target; \
    else \
      echo "x86_64-unknown-linux-musl" > /tmp/rust-target; \
    fi && \
    rustup target add "$(cat /tmp/rust-target)"
# Use musl-gcc as the linker for musl targets
ENV CARGO_TARGET_X86_64_UNKNOWN_LINUX_MUSL_LINKER=musl-gcc
ENV CARGO_TARGET_AARCH64_UNKNOWN_LINUX_MUSL_LINKER=musl-gcc
# Use thin LTO for Docker builds to reduce peak memory usage during linking.
# Full LTO (lto = true in Cargo.toml) can exceed the GitHub Actions runner
# memory limit when combined with resolver = "2" feature deduplication.
ENV CARGO_PROFILE_RELEASE_LTO=thin
WORKDIR /usr/src/cddl
COPY . .
RUN TARGET=$(cat /tmp/rust-target) && \
    cargo build --release --bin cddl --target "$TARGET" && \
    cp "target/$TARGET/release/cddl" /usr/local/bin/cddl

FROM scratch
COPY --from=builder /usr/local/bin/cddl /cddl
ENTRYPOINT [ "/cddl" ]