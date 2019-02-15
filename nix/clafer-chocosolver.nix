{ stdenv, lib, buildMaven, maven, runtimeShell, runtimeShellPackage
, branch ? "master" }:

# "Note: the name "choco-solver" refers to the Java library for Constraint
# Programming, whereas "chocosolver" (without the "-") is the name of this
# project. We might change the name in the future to avoid confusion."

let
  branchInfo = builtins.getAttr branch {
    master = {
      version = "0.4.4";
      infoFile = ./clafer-chocosolver-info-master.json;
      ref = "master";
      rev = "bc4cb12a23118d9e5d7f1003d83f2f8bed9cf179";
    };
    develop = {
      version = "0.4.4-develop";
      infoFile = ./clafer-chocosolver-info-develop.json;
      ref = "develop";
      rev = "217cd58a30d68c68c0b3222f85908197af2abbf6";
      patch = ./chocosolver-develop-fixes.patch;
    };
  };

  info = lib.importJSON branchInfo.infoFile;
  settings = (buildMaven branchInfo.infoFile).settings;

  version = branchInfo.version;

  jars = stdenv.mkDerivation rec {
    name = "clafer-chocosolver-${version}-jars";
    inherit version;

    src = builtins.fetchGit {
      url = "https://github.com/gsdlab/chocosolver.git";
      rev = branchInfo.rev;
      ref = branchInfo.ref;
    };

    buildInputs = [ maven ];

    patchPhase = if builtins.hasAttr "patch" branchInfo then ''
      patch -p1 -i ${branchInfo.patch}
    '' else "";

    buildPhase = "mvn --offline --settings ${settings} compile";

    installPhase = ''
      mvn --offline --settings ${settings} package -DskipTests
      mkdir $out/
      mv target/*.jar $out/
    '';
  };

in stdenv.mkDerivation rec {
  name = "clafer-chocosolver-${version}-${branch}";
  inherit version;

  buildInputs = [ jars runtimeShellPackage ];

  phases = [ "installPhase" ];

  installPhase = ''
    mkdir -p $out/bin $out/share/chocosolver

    echo '#!${runtimeShell}' >$out/bin/chocosolver
    echo "java -jar $out/share/chocosolver/chocosolver.jar \"\$@\"" >>$out/bin/chocosolver
    chmod +x $out/bin/chocosolver

    cp ${jars}/chocosolver-${version}-jar-with-dependencies.jar \
      $out/share/chocosolver/chocosolver.jar
  '';
}
