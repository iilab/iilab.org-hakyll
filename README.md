# iilab Hakyll website

Requirements:
 * ImageMagick 
 * JSMin

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

## To rebuild after adding or removing dependencies to the generator

 * edit the cabal file
 * regenerate the shell.nix file

```
$ cabal2nix --shell . > shell.nix
```

 * ```./rebuild.sh```

## TODO

 * Refactor like https://github.com/vbeffara/web-hakyll/blob/master/site.hs
 * 