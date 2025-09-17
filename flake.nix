{
  description = "The Llama Programming Language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane.url = "github:ipetkov/crane";
    advisory-db = {
      url = "github:rustsec/advisory-db";
      flake = false;
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    rust-overlay,
    crane,
    advisory-db,
    ...
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      overlays = [(import rust-overlay)];
      pkgs = import nixpkgs {inherit system overlays;};
      inherit (pkgs) lib;

      craneLib =
        (crane.mkLib pkgs)
        .overrideToolchain
        (pkgs.rust-bin.stable.latest.default.override
          {extensions = ["rust-src" "rust-analyzer"];});
      src = craneLib.cleanCargoSource ./.;

      commonArgs = {
        inherit src;
        strictDeps = true;
        buildInputs = [];
      };

      cargoArtifacts = craneLib.buildDepsOnly commonArgs;

      individualCrateArgs =
        commonArgs
        // {
          inherit cargoArtifacts;
          inherit (craneLib.crateNameFromCargoToml {inherit src;}) version;
          doCheck = true;
        };

      llamacFileset = lib.fileset.toSource {
        root = ./.;
        fileset = lib.fileset.unions [
          ./Cargo.toml
          ./Cargo.lock

          ./crates/llamac_ast
          ./crates/llamac_codegen_r6rs
          ./crates/llamac_parser
          ./crates/llamac_sexpr_fmt
          ./crates/llamac_typecheck
          ./crates/llamac_typed_ast
          ./crates/llamac_utils
          ./crates/llamac
        ];
      };

      llamac = craneLib.buildPackage (individualCrateArgs
        // {
          pname = "llamac";
          cargoExtraArgs = "-p llamac";
          buildInputs = [pkgs.racket];
          src = llamacFileset;
        });
    in {
      checks = {
        inherit llamac;

        llama-workspace-clippy = craneLib.cargoClippy (
          commonArgs
          // {
            inherit cargoArtifacts;
            cargoClippyExtraArgs = "--all-targets -- --deny warnings";
          }
        );

        llama-workspace-doc = craneLib.cargoDoc (commonArgs
          // {
            inherit cargoArtifacts;
            env.RUSTDOCFLAGS = "--deny warnings";
          });

        llama-workspace-fmt = craneLib.cargoFmt {inherit src;};

        llama-workspace-toml-fmt = craneLib.taploFmt {
          src = pkgs.lib.sources.sourceFilesBySuffices src [".toml"];
        };

        llama-workspace-audit = craneLib.cargoAudit {inherit src advisory-db;};

        llama-workspace-deny = craneLib.cargoDeny {inherit src;};
      };

      packages = {
        inherit llamac;
        default = llamac;
      };

      apps = {
        llamac = flake-utils.lib.mkApp {
          drv = llamac;
        };
      };

      devShells.default =
        craneLib.devShell.override {
          mkShell = pkgs.mkShell.override {stdenv = pkgs.stdenvAdapters.useMoldLinker pkgs.stdenv;};
        } {
          checks = self.checks.${system};
          packages = [pkgs.taplo];
        };
    });
}
