#!/usr/bin/env bash
stack build
cp `stack path --dist-dir`/build/ofx-nubank/ofx-nubank ../
