# iilab Hakyll website

## Setup the environment

export NIX_PATH 

## To rebuild the site after adding content

```
./site rebuild
```

## To rebuild the site after changing the generator

```
nix-shell -I ~ --command 'cabal build'
./site rebuild
```

or use ```./rebuild.sh```