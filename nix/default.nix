with builtins;

let pkgs = import <nixpkgs> {};

    tinycbor = pkgs.callPackage ./tinycbor.nix {};

    python3 = pkgs.python3.withPackages (ps:
      with ps;
      [ (ps.callPackage ./python3-cbor2.nix {})
      ]);

    haskellPackages = pkgs.haskell.packages.ghc844.override {
        overrides = self: super: {
            #process = self.callPackage ./process-1.6.3.0.nix {};
            data-stringmap = self.callPackage ./hackage/data-stringmap-1.0.1.1.nix {};
            json-builder = self.callPackage ./hackage/json-builder-0.3-for-ghc84.nix {};
            HaXml = self.callPackage ./hackage/HaXml-1.25.5.nix {};
            polyparse = self.callPackage ./hackage/polyparse-1.12.1.nix {};
            toml-parser = self.callPackage ./hackage/toml-parser-0.1.0.0.nix {};
        };
    };

    ghc = haskellPackages.ghcWithPackages (pkgs: with pkgs;
    let
      clafer = pkgs.callPackage ./clafer.nix {};
    in [
      clafer
      cborg
      graphviz
      syb union-find microlens-platform
      prettyprinter
      toml-parser
    ]);

    clafer_0_4_4 = haskellPackages.callPackage ./clafer-0.4.4.nix {};
    clafer_0_4_5 = haskellPackages.callPackage ./clafer-0.4.5.nix {};
    claferIG-haskell = haskellPackages.callPackage ./claferig-haskell.nix {
      clafer = clafer_0_4_5;
    };
    claferIG-java = pkgs.callPackage ./claferig-java.nix {};
    claferIG = pkgs.callPackage ./claferig.nix {
      inherit claferIG-haskell claferIG-java;
    };

    choco-solver = pkgs.callPackage ./choco-solver.nix {};
    clafer-chocosolver = pkgs.callPackage ./clafer-chocosolver.nix {
      branch = "master";
    };

    claferToolPath = pkgs.callPackage ./clafer-tool-path.nix {
      inherit clafer-chocosolver;
    };


in pkgs.mkShell {
    buildInputs = attrValues {
        inherit (pkgs) stdenv flex bison readline zlib;
        inherit tinycbor python3;

        inherit (pkgs) cabal2nix nix-prefetch-git;
        inherit (pkgs) graphviz;
        inherit (pkgs) alloy;
        inherit (pkgs) z3;

        #inherit (pkgs.openjdk8) jre;
        inherit (pkgs) openjdk8 maven;

        # Essential development tools, for working inside `nix-shell --pure`
        inherit (pkgs) git vim less hostname valgrind gdb pandoc unzip;

        inherit ghc;
        #inherit (haskellPackages) cabal-install;

        inherit claferToolPath claferIG;
        inherit clafer-chocosolver;
    };

    inherit claferToolPath;
    oldClafer = clafer_0_4_4;
}
