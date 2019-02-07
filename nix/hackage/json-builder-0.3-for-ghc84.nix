{ mkDerivation, base, blaze-builder, blaze-textual, bytestring
, containers, stdenv, text, unordered-containers, utf8-string
, vector
}:
mkDerivation {
  pname = "json-builder";
  version = "0.3";
  src = builtins.fetchGit {
    url = "https://github.com/nikomi/json-builder.git";
    ref = "ghc84";
    rev = "fef4700b53da502f8d70d2106c9f4b65e1a69b3f";
  };
  libraryHaskellDepends = [
    base blaze-builder blaze-textual bytestring containers text
    unordered-containers utf8-string vector
  ];
  homepage = "http://github.com/lpsmith/json-builder";
  description = "Data structure agnostic JSON serialization";
  license = stdenv.lib.licenses.bsd3;
}
