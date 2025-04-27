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
    flake-parts.url = "github:hercules-ci/flake-parts";
    git-hooks-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:cachix/git-hooks.nix";
    };
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-24.11";
    treefmt-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:numtide/treefmt-nix";
    };
  };

  outputs = inputs@{ flake-parts, nixpkgs, self, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      flake = {
        overlays.default = _final: prev: {
          haskellPackages = prev.haskellPackages.override {
            overrides = _hfinal: hprev: {
              combinat = hprev.callCabal2nix "combinat"
                (prev.fetchFromGitHub {
                  owner = "yurrriq";
                  repo = "combinat";
                  rev = "4aff8ad8721fc2c006230e0b9d7b2ffc87569186";
                  hash = "sha256-BDqa4076XnSpysKVlGMMBAzTYnWcddTRZ+8QVKIBI+o=";
                })
                { };
            };
          };

          myEmacs = prev.emacsWithPackagesFromUsePackage {
            alwaysEnsure = true;
            config = ./emacs.el;
          };
        };
      };

      imports = [
        inputs.git-hooks-nix.flakeModule
        inputs.treefmt-nix.flakeModule
      ];

      systems = [
        "x86_64-linux"
      ];

      perSystem = { config, pkgs, system, self', ... }: {
        _module.args.pkgs = import nixpkgs {
          overlays = [
            inputs.emacs-overlay.overlay
            self.overlays.default
          ];
          inherit system;
        };

        devShells.default = pkgs.mkShell {
          FONTCONFIG_FILE = pkgs.makeFontsConf {
            fontDirectories = [
              pkgs.nerd-fonts.iosevka
            ];
          };

          inputsFrom = [
            config.pre-commit.devShell
          ];

          nativeBuildInputs = with pkgs; [
            abcmidi
            cabal-install
            ccls
            delta
            ghc
            ghcid
            haskell-language-server
            haskellPackages.hpack
            haskellPackages.hlint
            haskellPackages.ormolu
            haskellPackages.pointfree
            just
            myEmacs
            nixd
            timidity
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
          programs = {
            clang-format.enable = true;
            deadnix.enable = true;
            hlint.enable = true;
            nixpkgs-fmt.enable = true;
            ormolu = {
              enable = true;
              ghcOpts = [
                "GHC2021"
              ];
            };
          };
          settings.formatter = {
            clang-format.options = [
              "-style=file"
            ];
          };
        };
      };
    };
}
