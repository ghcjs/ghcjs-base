{ modules = [{
    nonReinstallablePkgs =
    [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
      "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
      # ghcjs custom packages
      "ghcjs-prim" "ghcjs-th" "unix"
    ];
  }];
  shell.tools.cabal = {};
  shell.crossPlatforms = p: [ p.ghcjs ];
}
