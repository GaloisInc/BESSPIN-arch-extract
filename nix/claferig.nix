{ stdenv, alloy, claferIG-haskell, claferIG-java, unzip }:

stdenv.mkDerivation rec {
  name = "claferIG";
  phases = ["buildPhase" "installPhase"];

  buildInputs = [ claferIG-haskell claferIG-java alloy unzip ];

  buildPhase = ''
    unzip ${alloy}/share/alloy/alloy4.2_2015-02-22.jar
  '';

  # claferIG expects the alloyIG jar to be alongside the claferIG binary, and
  # expects libminisatprover.so to live in the `lib` subdirectory.
  installPhase = ''
    mkdir -p $out/bin $out/bin/lib
    cp ${claferIG-haskell}/bin/claferIG $out/bin/
    cp ${claferIG-java}/alloyIG.jar $out/bin/
    cp amd64-linux/libminisatprover.so $out/bin/lib/
  '';
}

