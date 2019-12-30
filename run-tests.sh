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

echo "Running the compilation tests."
"$EMACS" -Q -batch \
         -l clipetty.el \
         --eval "(setq byte-compile-error-on-warn ${ERROR_ON_WARN})" \
         -f batch-byte-compile \
         clipetty.el test/clipetty-test.el

echo "Running the package-lint tests."
"$EMACS" -Q -batch \
         --eval "$INIT_PACKAGE_EL" \
         -l package-lint.el \
         -f package-lint-batch-and-exit \
         clipetty.el test/clipetty-test.el || [ -n "${EMACS_LINT_IGNORE+x}" ]

echo "Running the checkdoc tests."
"$EMACS" -Q -batch \
         -l test/clipetty-test.el \
         -f clipetty-test-checkdoc-batch \
         clipetty.el test/clipetty-test.el

echo "Running Clipetty unit tests."
"$EMACS" -Q -batch \
         -l clipetty.el \
         -l test/clipetty-test.el \
         -f ert-run-tests-batch-and-exit
