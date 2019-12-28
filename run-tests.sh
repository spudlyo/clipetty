#!/bin/sh -e

EMACS="${EMACS:=emacs}"

INIT_PACKAGE_EL="(progn
  (require 'package)
  (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)
  (package-initialize))"

# Gotta install package-lint.
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         --eval '(package-refresh-contents)' \
         --eval "(unless (package-installed-p 'package-lint) (package-install 'package-lint))"

if [ -n "${EMACS_LINT_IGNORE+x}" ]; then
    ERROR_ON_WARN=nil
else
    ERROR_ON_WARN=t
fi

# Ensure everything compiles.
"$EMACS" -Q -batch \
         -l clipetty.el \
         --eval "(setq byte-compile-error-on-warn ${ERROR_ON_WARN})" \
         -f batch-byte-compile \
         clipetty.el test/clipetty-test.el test/clipetty-checkdoc.el

# Ensure everything passes checkdoc.
"$EMACS" -Q -batch \
         -l test/clipetty-checkdoc.el \
         -f clipetty-checkdoc-batch \
         clipetty.el test/clipetty-test.el test/clipetty-checkdoc.el

# Ensure package-lint is happy.
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l package-lint.el \
         -f package-lint-batch-and-exit \
         clipetty.el test/clipetty-test.el test/clipetty-checkdoc.el || [ -n "${EMACS_LINT_IGNORE+x}" ]

# Run the clipetty unit tests.
"$EMACS" -Q -batch \
         -l clipetty.el \
         -l test/clipetty-test.el \
         -f ert-run-tests-batch-and-exit
