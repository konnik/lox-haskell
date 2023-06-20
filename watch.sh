#!/bin/bash
find app/* *.lox crafting-interpreters.cabal | entr -c cabal run hlox -v0 -- program.lox