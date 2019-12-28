#!/bin/sh -e

EMACS="${EMACS:=emacs}"

if [ -n "${EMACS_LINT_IGNORE+x}" ]; then
    ERROR_ON_WARN=nil
else
    ERROR_ON_WARN=t
fi

"$EMACS" -Q -batch \
         -l clipetty.el \
         --eval "(setq byte-compile-error-on-warn ${ERROR_ON_WARN})" \
         -f batch-byte-compile \
         clipetty.el clipetty-test.el

"$EMACS" -Q -batch \
         -l clipetty.el \
         -l clipetty-test.el \
         -f ert-run-tests-batch-and-exit
