{ stdenv, racket, genRacketConfig, racket-rfc6455 }:

stdenv.mkDerivation rec {
  pname = "rosette";
  name = "racket-${pname}-${version}";
  version = "3.0";
  src = builtins.fetchGit {
    url = "https://github.com/emina/rosette.git";
    rev = "e4b56fae9492bf7287490d72772d97784154b565";
  };

  buildInputs = [racket racket-rfc6455];

  phases = ["unpackPhase" "setupPhase" "installPhase"];

  setupPhase = ''
    mkdir ../racket-config
    ${genRacketConfig [racket-rfc6455 "$out"]} >../racket-config/config.rktd
  '';

  installPhase = ''
    export HOME=$PWD/..
    export PLTCONFIGDIR=$PWD/../racket-config
    cd rosette
    raco pkg install \
      --no-setup --deps force \
      --scope-dir $out --copy -t dir -n ${pname} $PWD
    raco setup --no-user --no-pkg-deps --only --pkgs ${pname}
  '';
}
