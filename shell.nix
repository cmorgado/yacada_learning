{ pure ? false, source-repo-override ? { } }:
let
  packages = import ./. { inherit source-repo-override; };
  inherit (packages) pkgs yacada project;

in
  project.shellFor {
    withHoogle = false;

    nativeBuildInputs = with yacada; [
      hlint
      cabal-install
      haskell-language-server
      stylish-haskell
      pkgs.niv
      cardano-repo-tool
      pkgs.ghcid
      # HACK: This shouldn't need to be here.
      pkgs.lzma.dev
    ] ++ (pkgs.lib.optionals pure [
      pkgs.git
      pkgs.cacert
      pkgs.curl
      pkgs.jq
    ]);
  }
