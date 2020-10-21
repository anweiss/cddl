FROM ekidd/rust-musl-builder:1.47.0 AS builder
COPY . ./
RUN cargo b --release --bin cli

FROM scratch
COPY --from=builder /home/rust/src/target/x86_64-unknown-linux-musl/release/cli /cddl
ENTRYPOINT [ "/cddl" ]