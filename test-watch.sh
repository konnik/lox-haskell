#!/bin/bash
find app/* test.sh crafting-interpreters.cabal | entr -c ./test.sh