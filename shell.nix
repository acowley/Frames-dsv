{ compiler ? "ghc865"
, withHoogle ? true
, pkgs ? import (builtins.fetchTarball {
  name = "nixos-unstable-20.03.git.e4134747f56";
  url = https://github.com/nixos/nixpkgs/archive/e4134747f5666bcab8680aff67fa3b63384f9a0f.tar.gz;
  sha256 = "19pca75ds77c6nq6f3c6iky7m3alkpva2vbjas1c7xgngzy0mw4i";
}) {}
}:

let
  # pkgs = import <nixpkgs> {};
  # ghcide = (import (builtins.fetchTarball "https://github.com/hercules-ci/ghcide-nix/tarball/master") {}).ghcide-ghc865;
  ghcide = (import (builtins.fetchTarball "https://github.com/hercules-ci/ghcide-nix/archive/caab5c37a80c4b706ebdc5b50ad610cc96d6b9e2.tar.gz") {}).ghcide-ghc865;
  f = import ./default.nix;
  packageSet = pkgs.haskell.packages.${compiler};
  hspkgs = (
    if withHoogle then
      packageSet.override {
        overrides = (self: super: {
          # ghc = super.ghc // { withPackages = f: super.ghc.withHoogle (ps: f ps ++ [ps.cabal-install ]); };
          ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
          vinyl = pkgs.haskell.lib.dontCheck (super.callPackage ~/Projects/Vinyl {});
          Frames = pkgs.haskell.lib.dontBenchmark
                     (pkgs.haskell.lib.dontCheck
                       (super.callPackage ~/Projects/Frames {}));
          # intero = pkgs.haskell.lib.dontCheck (super.callPackage ~/src/intero {});
          # hw-rankselect = super.callHackage "hw-rankselect" "0.12.0.4" {};
          # hw-balancedparens = pkgs.haskell.lib.dontCheck super.hw-balancedparens;
          # hw-rankselect = pkgs.haskell.lib.dontCheck super.hw-rankselect;
          hw-balancedparens = pkgs.haskell.lib.dontCheck (self.callHackage "hw-balancedparens" "0.3.0.2" {});
          hw-rankselect = pkgs.haskell.lib.dontCheck (self.callHackage "hw-rankselect" "0.13.3.1" {});
          hw-dsv = self.callHackageDirect {
            pkg = "hw-dsv";
            ver = "0.4.0";
            sha256 = "1qnyy0scgbiddqxvmjmwy8hdw94wcy0xnyvfj1ay49i9kml0kvrv";
          } {};
          generic-lens = pkgs.haskell.lib.dontCheck (super.callHackage "generic-lens" "1.2.0.1" {});
          ghcWithPackages = self.ghc.withPackages;
        });
      }
      else packageSet
  );
  drv = hspkgs.callPackage f {};
  ghc = hspkgs.ghc.withHoogle (_: drv.passthru.getBuildInputs.haskellBuildInputs);
in
  # if pkgs.lib.inNixShell then drv.env else drv
  pkgs.mkShell {
    buildInputs = [ ghc ghcide pkgs.cabal-install ];
    shellHook = ''
      export NIX_GHC='${ghc}/bin/ghc'
      export NIX_GHCPKG='${ghc}/bin/ghc-pkg'
      export NIX_GHC_DOCDIR='${drv.compiler.doc}/share/doc/ghc/html'
      export NIX_GHC_LIBDIR='${ghc}/lib/${drv.compiler.name}'
      export HIE_HOOGLE_DATABASE='${ghc}/share/doc/hoogle/default.hoo'
    '';
  }
