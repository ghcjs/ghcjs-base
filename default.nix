{ compiler-nix-name ? "ghc8104"
}:
let
  sources = import ./nix/sources.nix {};
  haskellNix = import sources."haskellNix" {};
  project = haskellNix.hix.project { src = ./.; inherit compiler-nix-name; };
in project.projectCross.ghcjs

