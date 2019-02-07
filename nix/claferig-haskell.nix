{ mkDerivation, array, base, clafer, cmdargs, containers
, data-stringmap, directory, executable-path, filepath, haskeline
, HaXml, HUnit, json-builder, mtl, mtl-compat, parsec, process
, stdenv, string-conversions, tasty, tasty-hunit, tasty-th
, transformers, transformers-compat
}:
mkDerivation {
  pname = "claferIG";
  version = "0.4.5";
  src = builtins.fetchGit {
    url = "git@gitlab-ext.galois.com:ssith/claferig.git";
    rev = "f7290496e65676015b2583ecf2d0cd25f5cdcad7";
  };
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    array base clafer containers data-stringmap directory
    executable-path filepath haskeline HaXml json-builder mtl
    mtl-compat parsec process string-conversions transformers
    transformers-compat
  ];
  executableHaskellDepends = [
    base clafer cmdargs containers directory executable-path filepath
    haskeline mtl mtl-compat transformers transformers-compat
  ];
  testHaskellDepends = [
    array base clafer cmdargs directory filepath HUnit tasty
    tasty-hunit tasty-th transformers transformers-compat
  ];
  doCheck = false;
  homepage = "http://clafer.org";
  description = "claferIG is an interactive tool that generates instances of Clafer models";
  license = stdenv.lib.licenses.mit;
}
