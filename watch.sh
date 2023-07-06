#!/bin/bash

arg="${1:-program.lox}"

find app/* *.lox crafting-interpreters.cabal | entr -c cabal run hlox -v0 -- $arg