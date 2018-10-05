{ mkDerivation, base, Frames, vinyl, hw-dsv, pipes, vector, text
, bytestring, template-haskell, hspec, stdenv }:
mkDerivation {
  pname = "Frames-dsv";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base Frames vinyl hw-dsv pipes bytestring
                            text template-haskell vector hspec ];
  homepage = "https://github.com/acowley/Frames-dsv#readme";
  license = stdenv.lib.licenses.bsd3;
}
