#!/usr/bin/env bash

paths=( ".cabal/bin" "${HOME}/bine" )
for pathcomponent in "${paths[@]}"; do
    if ! echo "$PATH" | grep "$pathcomponent"; then
	mkdir -p $pathcomponent
	setpath="export PATH=${pathcomponent}:${PATH}"
        echo "$setpath" >>  ${HOME}/.profile
	"$setpath"
    fi
done

cabal update
cabal install cabal-install cabal

sudo apt-get install llvm

cabal sandbox init
cabal install arithmoi ./hstats-0.3
cabal configure
cabal build

mkdir -p "$HOME/bin"
exs=( checkReceivedSnapshots partialCheckReceivedSnapshots printReceivedSnapshots receivedSnapshots2staleness )
for p in ${exs[@]}; do
  ln -s "$(pwd)/dist/build/$p/$p" "${HOME}/bin"
done
