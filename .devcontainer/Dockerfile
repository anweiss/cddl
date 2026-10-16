FROM mcr.microsoft.com/devcontainers/rust:1-bookworm

RUN rustup default stable \
    && rustup component add rustfmt clippy \
    && rustup target add wasm32-unknown-unknown