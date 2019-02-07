{ mkDerivation, base, binary, containers, deepseq, ghc-heap-view
, HUnit, QuickCheck, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2
}:
mkDerivation {
  pname = "data-stringmap";
  version = "1.0.1.1";
  sha256 = "415d3000ef4d3dec1cd2df04fbad184aeea8ddc04cc4fe2401b943538b9f4b36";
  libraryHaskellDepends = [ base binary containers deepseq ];
  testHaskellDepends = [
    base containers deepseq ghc-heap-view HUnit QuickCheck
    test-framework test-framework-hunit test-framework-quickcheck2
  ];
  doCheck = false;
  description = "An efficient implementation of maps from strings to arbitrary values";
  license = stdenv.lib.licenses.mit;
}
