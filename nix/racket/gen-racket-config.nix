{ lib, racket }:
pkgs:

let files = lib.concatStringsSep " " pkgs;
in "${racket}/bin/racket ${./gen-racket-config.rkt} ${files} <${racket}/etc/racket/config.rktd"
