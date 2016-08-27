#!/usr/bin/env bash
if [ ! -d .stack-work ]; then
    stack init
fi
stack build
cp `stack path --dist-dir`/build/ofx-bb/ofx-bb ../

