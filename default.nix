{ mkDerivation, base, Frames, hw-dsv, pipes, vector, text, bytestring, stdenv }:
mkDerivation {
  pname = "Frames-dsv";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base Frames hw-dsv pipes bytestring text vector ];
  homepage = "https://github.com/acowley/Frames-dsv#readme";
  license = stdenv.lib.licenses.bsd3;
}
