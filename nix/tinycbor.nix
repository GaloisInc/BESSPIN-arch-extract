{ stdenv, fetchFromGitHub }:

stdenv.mkDerivation rec {
  name = "tinycbor-${version}";
  version = "0.5.2";

  src = fetchFromGitHub {
    owner  = "intel";
    repo   = "tinycbor";
    rev    = "v${version}";
    sha256 = "09q5f6wxvbpzxn530qqa9nk1s27mpa55bb96cbwl43rk84bhqihd";
  };

  makeFlags = [ "prefix=$(out)" ];

  meta = with stdenv.lib; {
    homepage = https://github.com/intel/tinycbor;
    description = "Concise Binary Object Representation (CBOR) Library";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}
