# Contributing to cddl-rs

Thank you for considering contributing to cddl-rs! This document provides guidelines and instructions for contributing to the project.

## Development Setup

### Prerequisites

- Rust 1.81.0 or later (minimum supported Rust version)
- Git

### Building from Source

```bash
cargo build
```

### Running Tests

```bash
cargo test --all -- --nocapture
```

## Code Quality Standards

### Code Formatting

This project uses `rustfmt` for code formatting. **All code must be formatted before committing.**

To format your code:

```bash
cargo fmt --all
```

To check if your code is properly formatted:

```bash
cargo fmt --all -- --check
```

The CI workflow will automatically check formatting and fail if code is not properly formatted.

### Linting

This project uses `clippy` for linting. Please address clippy warnings before submitting a PR:

```bash
cargo clippy --all
```

### Pre-Commit Hook (Optional but Recommended)

To automatically format your code before each commit, you can set up a Git pre-commit hook:

```bash
cat > .git/hooks/pre-commit << 'EOF'
#!/bin/sh
#
# Pre-commit hook that runs cargo fmt --check

echo "Running cargo fmt --check..."
if ! cargo fmt --all -- --check; then
    echo ""
    echo "Code is not formatted. Running cargo fmt --all to fix..."
    cargo fmt --all
    echo ""
    echo "Code has been formatted. Please review the changes and commit again."
    exit 1
fi
EOF

chmod +x .git/hooks/pre-commit
```

## Submitting Changes

1. Fork the repository
2. Create a new branch for your feature or bugfix
3. Make your changes
4. **Format your code with `cargo fmt --all`**
5. Run tests to ensure they pass
6. Commit your changes with a clear commit message
7. Push to your fork
8. Submit a Pull Request

## Pull Request Guidelines

- Ensure all tests pass
- Ensure code is properly formatted with `cargo fmt`
- Address any clippy warnings
- Provide a clear description of the changes
- Reference any related issues

## Questions?

If you have questions about contributing, please open an issue for discussion.
