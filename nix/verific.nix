{ stdenv, flex, yacc, zlib }:

let
  platform = "linux";

in stdenv.mkDerivation rec {
  name = "verific-${version}";
  version = "2019-02";

  src = builtins.fetchGit {
    url = "git@gitlab-ext.galois.com:ssith/verific.git";
    rev = "0747952da1bda23408a738921be965917a13f3c6";
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
