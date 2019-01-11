with builtins;

let pkgs = import <nixpkgs> {};

    tinycbor = pkgs.callPackage ./tinycbor.nix {};

    python3 = pkgs.python3.withPackages (ps:
      with ps;
      [ (ps.callPackage ./python3-cbor2.nix {})
      ]);

    haskellPackages = pkgs.haskell.packages.ghc822;

    ghc = haskellPackages.ghcWithPackages (pkgs: with pkgs;
      [ (pkgs.callPackage ./clafer.nix {})
      cborg
      diagrams graphviz diagrams-graphviz
      ]);


in pkgs.mkShell {
    buildInputs = attrValues {
        inherit (pkgs) stdenv flex bison readline zlib;
        inherit tinycbor python3;

        inherit (pkgs) cabal2nix nix-prefetch-git;
        inherit (pkgs) graphviz;

        # Essential development tools, for working inside `nix-shell --pure`
        inherit (pkgs) git vim less hostname valgrind gdb pandoc;

        inherit ghc;
        inherit (haskellPackages) cabal-install;
    };
}
