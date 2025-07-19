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
            overrides = _hfinal: hprev:
              let
                inherit (prev.lib.trivial) flip pipe;
                inherit (prev.haskell.lib) overrideSrc unmarkBroken;
              in
              {
                combinat = pipe hprev.combinat [
                  (
                    flip overrideSrc {
                      src = prev.fetchFromGitHub {
                        owner = "bkomuves";
                        repo = "combinat";
                        rev = "d5a22c2b7205455b3ddee681456d110d5cc7696e";
                        hash = "sha256-BDqa4076XnSpysKVlGMMBAzTYnWcddTRZ+8QVKIBI+o=";
                      };
                      version = "0.2.10.2";
                    }
                  )
                  unmarkBroken
                ];

                fast-digits = overrideSrc hprev.fast-digits {
                  src = prev.fetchFromGitHub {
                    owner = "Bodigrim";
                    repo = "fast-digits";
                    rev = "9a492fe4e10a298aa4471ee55fff1481005f891d";
                    hash = "sha256-0/VjBqsqN0GD1SyaM9mowPnCuCxNejcrrwt2HKA8Y2A=";
                  };
                  version = "0.3.2.1";
                };

                typelits-witnesses = unmarkBroken hprev.typelits-witnesses;
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

      perSystem = { config, lib, pkgs, system, self', ... }: {
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
            self'.packages.creating-rhythms
          ];

          nativeBuildInputs = with pkgs; [
            abcmidi
            cabal-install
            ghc
            ghcid
            haskell-language-server
            haskellPackages.hpack
            haskellPackages.hlint
            haskellPackages.ormolu
            haskellPackages.pointfree
            myEmacs
            nixd
            timidity
          ];

          shellHook = ''
            export LD_LIBRARY_PATH="${lib.makeLibraryPath [ pkgs.zlib ] }"
          '';
        };

        packages = {
          creating-rhythms =
            let
              src = lib.fileset.toSource {
                root = ./.;
                fileset = lib.fileset.unions [
                  ./VERSION
                  ./app
                  ./cabal.project
                  ./examples
                  ./package.yaml
                  ./src
                  ./test
                ];
              };

              pkg = pkgs.haskellPackages.callCabal2nix "creating-rhythms" src.outPath { };
            in
            pkgs.haskell.lib.overrideCabal pkg {
              haddockFlags = [
                # "--executables"
                # "--for-hackage"
                "--html-location='https://hackage.haskell.org/package/$pkgid/docs/'"
                "--hyperlink-source"
                "--quickjump"
                # "--use-unicode"
              ];
              librarySystemDepends = [
                pkgs.zlib
              ];
              testHaskellDepends = [
                pkgs.haskellPackages.doctest
              ];
            };

          default = self'.packages.creating-rhythms;
        };

        pre-commit.settings.hooks = {
          convco.enable = true;
          doctest = {
            enable = true;
            entry = "${lib.getExe pkgs.cabal-install} repl --repl-options='-w -Wdefault -XDataKinds' --with-compiler=doctest";
            name = "doctest";
            pass_filenames = false;
            types = [ "haskell" ];
          };
          editorconfig-checker.enable = true;
          treefmt.enable = true;
        };

        treefmt = {
          programs = {
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
        };
      };
    };
}
