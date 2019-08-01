FROM ekidd/rust-musl-builder:stable AS builder
COPY . ./
RUN cargo b --release --bin cddl

FROM scratch
COPY --from=builder /home/rust/src/target/x86_64-unknown-linux-musl/release/cddl /
CMD ["/cddl"]