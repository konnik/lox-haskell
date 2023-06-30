#!/bin/bash

original_dir=$(pwd)
cd `dirname "$0"` || exit -1

cabal run hlox -v0 -- ../craftinginterpreters/$1
exitcode=$?

cd "$original_dir" || exit -1

exit $exitcode
