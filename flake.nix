{
  description = "haskell-stack-template-rust-ffi";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs =
    { flake-parts, ... }@inputs:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
      perSystem =
        { lib, pkgs, ... }:
        let
          # needs to match Stackage LTS version from stack.yaml snapshot
          hPkgs = pkgs.haskell.packages.ghc965;

          # wrap Stack to use the flake Haskell packages
          stack-wrapped = pkgs.symlinkJoin {
            name = "stack";
            paths = [ pkgs.stack ];
            buildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/stack \
                --add-flags "--no-nix --system-ghc --no-install-ghc"
            '';
          };

          myDevTools = [
            # GHC compiler in the desired version (will be available on PATH)
            hPkgs.ghc
            # Continuous terminal Haskell compile checker
            hPkgs.ghcid
            # Haskell formatter
            hPkgs.ormolu
            # Haskell codestyle checker
            hPkgs.hlint
            # Lookup Haskell documentation
            hPkgs.hoogle
            # LSP server for editor
            hPkgs.haskell-language-server
            # auto generate LSP hie.yaml file from cabal
            hPkgs.implicit-hie
            # Haskell refactoring tool
            hPkgs.retrie
            # Haskell build tool
            stack-wrapped

            # External C library needed by some Haskell packages
            pkgs.pkg-config
            pkgs.zlib

            # Rust
            pkgs.cargo
            pkgs.rust-analyzer
          ];
        in
        {
          devShells.default = pkgs.mkShell {
            buildInputs = myDevTools;
            env = {
              LD_LIBRARY_PATH = lib.makeLibraryPath myDevTools;
              RUST_SRC_PATH = "${pkgs.rust.packages.stable.rustPlatform.rustLibSrc}";
            };
          };

          formatter = pkgs.nixfmt-rfc-style;

          packages.default =
            let
              os = if pkgs.stdenv.isDarwin then "osx" else "linux";
              libExt = if pkgs.stdenv.isDarwin then "dylib" else "so";
              libPath =
                {
                  osx = "DYLD_LIBRARY_PATH : \"$out/lib/links\"";
                  linux = "LD_LIBRARY_PATH : \"$out/lib/links\"";
                }
                ."${os}";
            in
            (hPkgs.mkDerivation {
              pname = "template-rust-ffi";
              version = inputs.self.shortRev or inputs.self.dirtyShortRev;
              src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;

              executableHaskellDepends = [ ];
              libraryHaskellDepends = [ ];
              testHaskellDepends = [
                hPkgs.hspec
                hPkgs.QuickCheck
              ];

              license = lib.licenses.mit;
            }).overrideAttrs
              (prev: {
                nativeBuildInputs = (prev.nativeBuildInputs or [ ]) ++ [
                  pkgs.cargo
                  pkgs.rustPlatform.cargoSetupHook
                  pkgs.makeWrapper
                ];

                cargoDeps = pkgs.rustPlatform.importCargoLock { lockFile = ./rust/Cargo.lock; };
                cargoRoot = "rust";

                postInstall = ''
                  cp -f ./rust/target/release/librust.${libExt} $out/lib/links/librust.${libExt}

                  wrapProgram $out/bin/${prev.mainProgram or prev.pname} \
                    --prefix ${libPath}
                '';
              });
        };
    };
}
