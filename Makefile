.PHONY: help fmt fmt-check clippy test build clean

help: ## Show this help message
	@echo 'Usage: make [target]'
	@echo ''
	@echo 'Available targets:'
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "  \033[36m%-15s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

fmt: ## Format all Rust code
	cargo fmt --all

fmt-check: ## Check if code is formatted (used in CI)
	cargo fmt --all -- --check

clippy: ## Run clippy linter
	cargo clippy --all

test: ## Run all tests
	cargo test --all -- --nocapture

build: ## Build the project
	cargo build

clean: ## Clean build artifacts
	cargo clean

pre-commit: fmt-check clippy test ## Run all pre-commit checks
