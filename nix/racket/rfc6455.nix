{ stdenv, racket, genRacketConfig }:

stdenv.mkDerivation rec {
  pname = "rfc6455";
  name = "racket-${pname}-${version}";
  version = "20160918";
  src = builtins.fetchGit {
    url = "https://github.com/tonyg/racket-rfc6455.git";
    rev = "ba4fa215e3ec71741bd00bb9804c76fc2b3b9e2e";
  };

  buildInputs = [racket];

  phases = ["unpackPhase" "setupPhase" "installPhase"];

  setupPhase = ''
    mkdir ../racket-config
    ${genRacketConfig ["$out"]} >../racket-config/config.rktd
  '';

  installPhase = ''
    export HOME=$PWD/..
    export PLTCONFIGDIR=$PWD/../racket-config
    raco pkg install \
      --no-setup --deps force \
      --scope-dir $out --copy -t dir -n ${pname} $PWD
    raco setup --no-user --no-pkg-deps --only --pkgs ${pname}
  '';
}
