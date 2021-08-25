let
  pkgs = import (builtins.fetchGit {
    name = "emacs-revision-26.3";
    url = "https://github.com/NixOS/nixpkgs/";                       
    ref = "refs/heads/nixpkgs-unstable";                     
    rev = "bed08131cd29a85f19716d9351940bdc34834492";       
  }) {};
  emacsWithPackages = with pkgs; (emacsPackagesNgGen emacs26).emacsWithPackages;
  run-test = import ./run-test.nix {
    inherit emacsWithPackages;
  };
in run-test
