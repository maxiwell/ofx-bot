#!/usr/bin/env bash
cabal sandbox init
cabal install --only-dependencies
cabal build
cp ./dist/build/csv-nubank/csv-nubank ../
