{ stdenv, alloy, openjdk8 }:

let alloyJar = "${alloy}/share/alloy/alloy4.2_2015-02-22.jar";
in stdenv.mkDerivation rec {
  name = "claferIG-java";
  phases = ["unpackPhase buildPhase installPhase"];

  src = builtins.fetchGit {
    url = "git@gitlab-ext.galois.com:ssith/claferig.git";
    rev = "f7290496e65676015b2583ecf2d0cd25f5cdcad7";
  };

  buildInputs = [ alloy openjdk8 ];

  # Easier to just replicate the build commands here than to patch the Makefile
  # to put the right alloy jar in the javac classpath
  buildPhase = ''
    mkdir -p dist/javabuild
    javac -cp ${alloyJar} -d dist/javabuild \
      src/org/clafer/ig/*.java \
      src/edu/mit/csail/sdg/alloy4compiler/parser/*.java
    cat >src/manifest <<EOF
    Main-Class: org.clafer.ig.AlloyIG
    Class-Path: ${alloyJar}
    EOF
    jar cfm alloyIG.jar src/manifest -C dist/javabuild org/clafer/ig/ -C dist/javabuild edu
  '';

  installPhase = ''
    mkdir -p $out
    cp alloyIG.jar $out/alloyIG.jar
  '';
}

