#!/usr/bin/env bash
git submodule init
git submodule update
cabal sandbox init
cabal sandbox add-source itau-lib
cabal install --only-dependencies
cabal build
cp ./dist/build/ofx-itau/ofx-itau ../
