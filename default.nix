{ source-repo-override ? { } }:
########################################################################
# default.nix -- The top-level nix build file for yacada.
#
# This file defines various attributes that are used for building and
# developing yacada.
#
########################################################################

let
  # Here a some of the various attributes for the variable 'packages':
  #
  # { pkgs
  #   yacada: {
  #     haskell: {
  #       project # The Haskell project created by haskell-nix.project
  #       packages # All the packages defined by our project, including dependencies
  #       projectPackages # Just the packages in the project
  #     }
  #     hlint
  #     cabal-install
  #     stylish-haskell
  #     haskell-language-server
  #   }
  # }
  packages = import ./nix { inherit source-repo-override; };

  inherit (packages) pkgs yacada;
  project = yacada.haskell.project;
in
{
  inherit pkgs yacada;

  inherit project;
}
