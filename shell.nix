{ compiler ? "ghc843"
, withHoogle ? true
}:

let
  pkgs = import <nixpkgs> {};
  f = import ./default.nix;
packageSet = pkgs.haskell.packages.${compiler};
  hspkgs = (
    if withHoogle then
      packageSet.override {
        overrides = (self: super: {
        ghc = super.ghc // { withPackages = f: super.ghc.withHoogle (ps: f ps ++ [ps.intero ps.cabal-install ]); };
          vinyl = pkgs.haskell.lib.dontCheck (super.callPackage ~/Projects/Vinyl {});
          Frames = pkgs.haskell.lib.dontBenchmark
                     (pkgs.haskell.lib.dontCheck
                       (super.callPackage ~/Projects/Frames {}));
          intero = pkgs.haskell.lib.dontCheck (super.callPackage ~/src/intero {});
          hw-rankselect = super.callHackage "hw-rankselect" "0.12.0.4" {};
          ghcWithPackages = self.ghc.withPackages;
        });
      }
      else packageSet
  );
  drv = hspkgs.callPackage f {};
in
  if pkgs.lib.inNixShell then drv.env else drv
