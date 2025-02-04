{
  description = "Description for the project";

  inputs = {
    emacs-overlay = {
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs-stable";
      };
      url = "github:nix-community/emacs-overlay";
    };
    fenix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/fenix";
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-24.05";
    pre-commit-hooks-nix = {
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs-stable";
      };
      url = "github:cachix/git-hooks.nix";
    };
    treefmt-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:numtide/treefmt-nix";
    };
  };

  outputs = inputs@{ flake-parts, nixpkgs, self, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      flake = {
        overlays.default = _final: prev: {
          myEmacs = prev.emacsWithPackagesFromUsePackage {
            alwaysEnsure = true;
            config = ./emacs.el;
          };
        };
      };

      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
        inputs.treefmt-nix.flakeModule
      ];

      systems = [
        "x86_64-linux"
      ];

      perSystem = { config, lib, pkgs, system, self', ... }: {
        _module.args.pkgs = import nixpkgs {
          overlays = [
            inputs.emacs-overlay.overlay
            inputs.fenix.overlays.default
            self.overlays.default
          ];
          inherit system;
        };

        devShells.default = pkgs.mkShell {
          FONTCONFIG_FILE = pkgs.makeFontsConf {
            fontDirectories = [
              (pkgs.nerdfonts.override { fonts = [ "Iosevka" ]; })
            ];
          };

          RUST_BACKTRACE = 1;

          inputsFrom = [
            config.pre-commit.devShell
          ];

          nativeBuildInputs = with pkgs; [
            cabal-install
            ccls
            (
              fenix.complete.withComponents [
                "cargo"
                "clippy"
                "rust-src"
                "rustc"
                "rustfmt"
              ]
            )
            gcc
            ghc
            ghcid
            haskell-language-server
            haskellPackages.hpack
            haskellPackages.hlint
            haskellPackages.ormolu
            haskellPackages.pointfree
            myEmacs
            nixd
            rust-analyzer-nightly
          ];
        };

        packages = {
          creating-rhythms = pkgs.haskellPackages.callCabal2nix
            "creating-rhythms"
            (pkgs.nix-gitignore.gitignoreSource [ ] ./.)
            { };

          default = self'.packages.creating-rhythms;
        };

        pre-commit.settings.hooks = {
          convco.enable = true;
          editorconfig-checker.enable = true;
          treefmt.enable = true;
        };

        treefmt = {
          projectRootFile = ./flake.nix;
          programs = {
            clang-format.enable = true;
            deadnix.enable = true;
            hlint.enable = true;
            nixpkgs-fmt.enable = true;
            ormolu.enable = true;
            rustfmt = {
              enable = true;
              package = pkgs.fenix.complete.rustfmt;
            };
          };
          settings.formatter = {
            clang-format.options = [
              "-style=file"
            ];
            rustfmt.options = lib.mkForce [
              "--edition"
              "2021"
            ];
          };
        };
      };
    };
}
