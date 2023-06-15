#!/bin/bash
find app/* hello.lox crafting-interpreters.cabal | entr -c cabal run hlox -v0 -- hello.lox