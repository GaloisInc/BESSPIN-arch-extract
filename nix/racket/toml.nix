{ stdenv, racket, genRacketConfig, parsack }:

stdenv.mkDerivation rec {
  pname = "toml";
  name = "racket-${pname}-${version}";
  version = "0.1";
  src = builtins.fetchGit {
    url = "https://github.com/greghendershott/toml.git";
    rev = "0321b8a99b950f2cddeae8227f79340c37df0533";
  };

  buildInputs = [racket parsack];

  phases = ["unpackPhase" "patchPhase" "setupPhase" "installPhase"];

  patchPhase = ''
    ls
    pwd
    rm toml-test.rkt
  '';

  setupPhase = ''
    mkdir ../racket-config
    ${genRacketConfig [parsack "$out"]} >../racket-config/config.rktd
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
