#!/usr/bin/env bash

setpath="export PATH=${HOME}/.cabal/bin:${PATH}"
echo "$setpath" >  ${HOME}/.profile

$setpath

cabal update
cabal install cabal-install cabal

sudo apt-get install llvm

cabal sandbox init
cabal install arithmoi ./hstats-0.3
cabal configure
cabal build
