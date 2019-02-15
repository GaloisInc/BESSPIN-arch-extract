{ stdenv, alloy, clafer-chocosolver }:

stdenv.mkDerivation rec {
  name = "clafer-tool-path";
  phases = ["installPhase"];

  buildInputs = [ alloy clafer-chocosolver ];

  installPhase = ''
    mkdir -p $out
    cp ${alloy}/share/alloy/alloy4.2_2015-02-22.jar $out/alloy4.2.jar
    cp ${clafer-chocosolver}/share/chocosolver/chocosolver.jar $out/chocosolver.jar
  '';
}

