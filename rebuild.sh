#!/bin/bash
set -e
nix-shell --command 'cabal build'
./site clean
./site watch