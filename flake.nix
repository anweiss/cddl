{
  description = "A Rust implementation of the Concise data definition language (CDDL)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    nixpkgs,
    rust-overlay,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        overlays = [(import rust-overlay)];
        pkgs = import nixpkgs {
          inherit system overlays;
        };

        rustToolchain = pkgs.rust-bin.stable.latest.default;
        nativeBuildInputs = with pkgs; [
          rustToolchain
          pkg-config
        ];

        buildInputs = with pkgs; [
          glibc
        ];
      in {
        packages.default = pkgs.rustPlatform.buildRustPackage {
          inherit nativeBuildInputs buildInputs;
          pname = "cddl";
          version = "0.9.5";

          src = ./.;

          cargoLock = {
            lockFile = ./Cargo.lock;
            allowBuiltinFetchGit = true;
          };

          meta = with pkgs.lib; {
            description = "Parser for the Concise data definition language (CDDL)";
            homepage = "https://cddl.anweiss.tech";
            license = licenses.mit;
            maintainers = ["Andrew Weiss <andrew.weiss@outlook.com>"];
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = [
            nativeBuildInputs
            buildInputs
          ];
        };
      }
    );
}
