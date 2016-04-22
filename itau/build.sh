#!/usr/bin/env bash
git submodule init
git submodule update
stack build
cp `stack path --dist-dir`/build/ofx-itau/ofx-itau ../

