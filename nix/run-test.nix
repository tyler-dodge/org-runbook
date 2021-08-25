{ emacsWithPackages }:
let
  pkgs = import <nixpkgs> {};
  versioned_emacs = emacsWithPackages (epkgs: with epkgs; [
    ivy
    ert-async
    el-mock
    ert-runner
    uuid
    s
    deferred
    ht
    dash
    mustache
    projectile
    undercover
  ]);
in derivation rec {
  name = "org-runbook";
  baseInputs = [];
  builder = "${pkgs.bash}/bin/bash";
  args = [ ./builder.sh ];
  setup = ./setup.sh;
  buildInputs = [
    versioned_emacs pkgs.coreutils];
  emacs = versioned_emacs;
  org_runbook = ../org-runbook.el;
  org_runbook_ivy = ../org-runbook-ivy.el;
  test_target = ../test;
  system = builtins.currentSystem;
}

  
