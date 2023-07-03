#!/bin/bash

#TESTTAG=chap09_control
TESTTAG=chap10_functions
#


# chap05_representing
# chap06_parsing
# chap07_evaluating
# chap08_statements
# chap09_control
# chap10_functions
# chap11_resolving
# chap12_classes
# chap13_inheritance

set -e
cabal build 
set +e

original_dir=$(pwd)
cd ../craftinginterpreters/ || exit -1


dart tool/bin/test.dart $TESTTAG --interpreter ../lox-haskell/run.sh
exitcode=$?

cd "$original_dir" || exit -2

exit $exitcode

