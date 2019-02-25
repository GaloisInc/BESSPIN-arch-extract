{ stdenv, flex, yacc, zlib }:

let
  platform = "linux";

in stdenv.mkDerivation rec {
  name = "verific-${version}";
  version = "2019-01";

  src = builtins.fetchGit {
    url = "git@gitlab-ext.galois.com:ssith/verific.git";
    rev = "92b032e9a4eb01af6bede9a5a7f8027693e9367b";
  };

  buildInputs = [ flex yacc zlib ];

  buildPhase = ''
    make -C util
    make -C containers
    make -C database
    make -C verilog
  '';

  installPhase = ''
    mkdir -p $out $out/include/verific $out/lib
    for d in util containers database verilog; do
      cp $d/*.h $out/include/verific/
      cp $d/$d-${platform}.a $out/lib/libverific_$d.a
    done
  '';
}
