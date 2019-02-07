{ stdenv, alloy }:

stdenv.mkDerivation rec {
  name = "clafer-tool-path";
  phases = ["installPhase"];

  buildInputs = [ alloy ];

  installPhase = ''
    mkdir -p $out
    cp ${alloy}/share/alloy/alloy4.2_2015-02-22.jar $out/alloy4.2.jar
  '';
}

