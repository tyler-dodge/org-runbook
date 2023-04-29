let
  org-runbook-target = {
    name = "org-runbook.el";
    file = ../org-runbook.el;
  };
  org-runbook-ivy-target = {
    name = "org-runbook-ivy.el";
    file = ../org-runbook-ivy.el;
  };
in {
  name = "org-runbook";
  targets = [
    org-runbook-target
    org-runbook-ivy-target
  ];
}
