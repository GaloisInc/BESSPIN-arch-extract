{ stdenv, racket, genRacketConfig }:

stdenv.mkDerivation rec {
  pname = "parsack";
  name = "racket-${pname}-${version}";
  # 0.4 is the latest, but `toml` requires 0.3
  version = "0.3";
  src = builtins.fetchGit {
    url = "https://github.com/stchang/parsack.git";
    rev = "148957939b082ad88a6a12db48a35c352cfeddb6";
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
