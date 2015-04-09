#!/bin/bash
set -e
nix-shell -I ~ --command 'cabal build'
./site clean
./site watch