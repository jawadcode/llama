{
  description = "The Llama Programming Language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    rust-overlay,
    crane,
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
          ./llamac_ast
          ./llamac_parser
          ./llamac_sexpr_fmt
          ./llamac_typecheck
          ./llamac_typed_ast
          ./llamac_utils
          ./llamac
        ];
      };

      llamac = craneLib.buildPackage (individualCrateArgs
        // {
          pname = "llamac";
          cargoExtraArgs = "-p llamac";
          src = llamacFileset;
        });
    in {
      checks = {
        inherit llamac;

        llama-workspace-clippy = craneLib.cargoClippy (commonArgs
          // {
            inherit cargoArtifacts;
            cargoClippyExtraArgs = "--all-targets -- --deny warnings";
          });

        llama-workspace-fmt = craneLib.cargoFmt {inherit src;};

        llama-workspace-deny = craneLib.cargoDeny {inherit src;};
      };

      packages = {
        default = llamac;
      };

      apps = {
        llamac = flake-utils.lib.mkApp {
          drv = llamac;
        };
      };

      devShells.default = craneLib.devShell {
        checks = self.checks.${system};
        packages = [pkgs.taplo];
      };
    });
}
