{ mkDerivation, alex, array, base, happy, stdenv, text, time }:
mkDerivation {
  pname = "toml-parser";
  version = "0.1.0.0";
  sha256 = "da81ecf51a9f814aef58d24f3ab95e46a57076a8eb3d2989c58ce204c0a0365c";
  revision = "1";
  editedCabalFile = "0w5vpr6gh0671znv3k90gy9fzjvxzn3g7bir2c6z27ks6y39w0qf";
  libraryHaskellDepends = [ array base text time ];
  libraryToolDepends = [ alex happy ];
  homepage = "https://github.com/glguy/toml-parser";
  description = "Parser for the TOML configuration language";
  license = stdenv.lib.licenses.isc;
}
