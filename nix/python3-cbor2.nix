{ lib, fetchPypi, buildPythonPackage }:

buildPythonPackage rec {
  version = "4.1.0";
  pname = "cbor2";

  src = fetchPypi {
    inherit pname version;
    sha256 = "0b1p01wgszk9f8lk689wg6b7758zwdfn1jb3vdqv4ldwls7im2m4";
  };

  meta = {
    homepage = "https://github.com/agronholm/cbor2";
    description = "Pure Python CBOR (de)serializer with extensive tag support";
    license = lib.licenses.mit;
  };

  patches = [
    ./python3-cbor2-ascii-setup-cfg.patch
  ];
}
