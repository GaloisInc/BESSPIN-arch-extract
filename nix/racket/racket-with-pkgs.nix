{ stdenv
, lib
, racket
, genRacketConfig
}:
racketPkgs:

stdenv.mkDerivation rec {
  name = "racket-with-packages";
  phases = ["installPhase"];

  buildInputs = [racket];

  installPhase = ''
    mkdir -p $out/bin $out/etc/racket

    ${genRacketConfig racketPkgs} >$out/etc/racket/config.rktd

    echo '#!/bin/sh' >$out/bin/racket
    echo "exec ${racket}/bin/racket -G $out/etc/racket \"\$@\"" >>$out/bin/racket
    chmod +x $out/bin/racket
  '';
}

