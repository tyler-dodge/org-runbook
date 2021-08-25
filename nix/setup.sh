unset PATH
for p in $baseInputs $buildInputs; do
  export PATH=$p/bin${PATH:+:}$PATH
done

function buildPhase() {
    ln -s $test_target test
    mkdir home
    export HOME=home/
    ${emacs}/bin/emacs -q --version
    ${emacs}/bin/emacs -q -batch -l $org_runbook -l $org_runbook_ivy -l ert-runner
    mkdir $out
}

function genericBuild() {
  buildPhase
}
