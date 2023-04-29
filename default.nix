let
  pkgs = import <nixpkgs> {};
  lib = pkgs.fetchFromGitHub {
    owner = "tyler-dodge";
    repo = "emacs-package-nix-build";
    rev = "eb109da5900436c7b2ec2a61818a0fc7e2fdce8a";
    hash = "sha256-Iq9VMffjSumE7imFMvHqb0Ydjrfh25fQDD+COBzdt68=";
  };
  org-runbook-target = {
    name = "org-runbook.el";
    file = ./org-runbook.el;
  };
  org-runbook-ivy-target = {
    name = "org-runbook-ivy.el";
    file = ./org-runbook-ivy.el;
  };
in import lib {
  package = {
    name = "org-runbook";
    test_target = ./test;
    targets = [
      org-runbook-target
      org-runbook-ivy-target
    ];
  };
}
