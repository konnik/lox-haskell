#!/bin/bash

TESTTAG=chap09_control

set -e
cabal build 
set +e

original_dir=$(pwd)
cd ../craftinginterpreters/ || exit -1


dart tool/bin/test.dart $TESTTAG --interpreter ../lox-haskell/run.sh
exitcode=$?

cd "$original_dir" || exit -2

exit $exitcode

