---
title: Nix + OSX + HaskellNG = Dependency Paradise ?
slug: 2015-03-27-nix-osx-haskellng-hakyll
format: tech
tags: functional programming, static site generator, haskell, hakyll, nix 
author: jun
images: haskell.jpg
---

I've recently started to learn Haskell and have been in the books for a few months, so didn't yet have to experience the practical side of what comes with the workflow of working with Haskell. 

I decided to try getting my hands dirty with a simple project, migrating the iilab website to the Haskell static site generator [Hakyll](http://jaspervdj.be/hakyll/). In this post I look at how I setup my development environment, and hope to ease the pain for others that are used to the simple ```npm install``` style workflows by looking at the latest in how to deal with Haskell dependency problems also known as "cabal hell". 

<!--more-->

Last year after a difficult experience with Jekyll for the Amnesty International Panic Button website, I looked around other approaches to do static website generation and, given I was into JS at the time and like the small modules approach, I settled on metalsmith, which seemed to have a clever design, based on a configurable pipeline, that seemed flexible enough. When I installed it, I did have to hack a few modules to make them do what I wanted, but it seemed straighforward enough. More recently I wanted to do a bit of freshening up on our website, and was looking at building a workflow with css/js concatenation and minimisation. Given how metalsmith was supposed to be this versatile, build pipeline, I thought it would be simple but I got turned off by the lack of clarity, and things started breaking pretty badly when I tried to upgrade modules to latest versions.

That's when, in a familiar sequence of events, [well captured](http://chriswarbo.net/blog/2014-07-14-nixos.html) as

 - Try to do something simple 
 - Fail 
 - Go down a rabbit hole in an attempt to fix it

I recursed through a few rabbit holes after my metalsmith/npm woes by trying Hakyll, running into cabal hell, decided to try Nix, saw that it was a bit bleeding edge for OSX, managed to go through that more or less, until I bumped into a compilation problem with the haskellPackages and was helped by the nice folks on the ```##nix-darwin``` IRC channel and pointed to the even more bleeding edge HaskellNG Nix approach which of course, is how deep the rabbit hole goes.

In short, here's how the pieces fit together:

 - Nix provides **declarative** and **deterministic** package management. Read about [why you should care](https://www.domenkozar.com/2014/03/11/why-puppet-chef-ansible-arent-good-enough-and-we-can-do-better/).
<br><br>
 - HaskellNG draws on Nix to make Haskell package management **declarative** and goes beyond the current ```cabal sandbox``` approach by allowing:

    - reuse of already compiled binaries (instead of having to recompile separately in every sandbox) and
    - dealing with dependencies at the compiler (and cabal) level, therefore [allowing to test bleeding edge compiler features](https://mail.haskell.org/pipermail/haskell-cafe/2015-March/118817.html) without fear of messing up your development environment.

## Nix

Nix is a package manager which is also the foundation of a linux distribution called NixOS which uses the philosophy of Nix through and through to provide a well regarded linux distribution. What Nix offers is a reliable way to manage dependencies and escape the various type of hells that the problem creates (DLL hell, cabal hell, the type of spagetthi that systems folk don't want...). It does this by knowing the complete dependency tree of all the packages it manages. This reminds me of docker's declarative approach to system configuratio, except that I think that docker's layered approach at the file system level brings an interesting lower level dimension. In any case, with Nix, several binaries with a different set of dependencies can coexist on your system, and more importantly, use the functional immutable approach to Nix package dependencies in order to keep your environment and yourself health, sane and functional.

A few things that Nix does is make sure dependencies are exact (by using binary hashes in a clever way - which includes all the dependency tree of the binary) and only reuse them when necessary. This means that the nix store holds a range of binary images which might be used by different other binaries and can coexist happily.

Nix then offers a number of wrapping tools to make these binaries available, for instance, when installing with ```nix-env -iA nixpkgs.somePackage``` you're installing somePackage inside your **user environment** which is in fact symbolic links to the nix store defined in your ```~/.nix-profile``` directory.

When using ```nix-shell``` you're using a **project environment** which might have different dependencies.

## Nix on OSX

The great thing, given my current dependency on this fruity old laptop and its pretty UX, is that Nix is available on OSX. I took me a little bit of meandering to find out because there are [outdated blog posts](http://www.chrisjr.org/posts/2014/12/22/nix-osx-haskell/) on the web (which give workarounds from **before** the OSX changes were marged into the Nix master branch) that [these instructions on the NixOS Wiki about **Using Nix on 10.9 and 10.10**](https://nixos.org/wiki/Nix_on_OS_X#Using_Nix_on_10.9_and_10.10) actually work just fine. I did bump into a problem because I started installing from the wrong git repo, a number of weird errors came up after switching to the correct instructions which meant that I had to restart from scratch and remove all the nix directories (/nix/store and ˜/.nix-profile).

## HaskellNG on Nix

When I first bumped into [this post about the new way](http://article.gmane.org/gmane.linux.distributions.nixos/15513) to do Haskell with Nix, I thought : "Oh, this looks great but very bleeding edge and I'm down several rabbit holes already so let's look at it later". So I went my merry way installing following [the official Nix instructions on the wiki](https://nixos.org/wiki/Haskell). But then after installing the Haskell toolkit (apparently the [Nix crowd is too cool for the Haskell Platform](https://www.mail-archive.com/nix-dev@lists.science.uu.nl/msg13622.html)) I found out that the Nix tribe on OSX seemed to have already left the old ways behind and migrated to HaskellNG. I found out because I ran into this error:

```
installing ‘haskell-hakyll-ghc7.8.4-4.6.1.1-shared’
error: Package ‘util-linux-2.26’ in ‘/../dev/nixpkgs/pkgs/os-specific/linux/util-linux/default.nix:54’ is not supported on ‘x86_64-darwin’, refusing to evaluate.
```

Here are a few reasons why HaskellNG is the way to go, as summarised in the [[Nix-dev] A Journey into the Haskell NG infrastructure: Part I](http://lists.science.uu.nl/pipermail/nix-dev/2015-January/015591.html) post:

 - The NG package set contains the latest version of every package available from Hackage
 - Haskell NG supports GHC 7.10.x.
 - Haskell NG allows you to configure the hell out of your installation.

## Step by step

To recap, here's what it took to set things up:
 - following [these steps to install Nix on OSX 10.10](https://nixos.org/wiki/Nix_on_OS_X#Using_Nix_on_10.9_and_10.10)
 - adapting [these steps](http://thread.gmane.org/gmane.linux.distributions.nixos/15035/focus=15161) to haskellng 
 - and [this description of the haskell cabal workflow](http://wiki.ocharles.org.uk/Nix).

### Install Nix on OSX

A [slightly more concise version of the official Nix wiki instructions](https://nixos.org/wiki/Nix_on_OS_X#Using_Nix_on_10.9_and_10.10). ** I'd recommend heading over there for the latest!!! **

First, make sure to install Xcode or Command Line Tools (or both).

```
$ curl https://nixos.org/nix/install | sh
$ . $HOME/.nix-profile/etc/profile.d/nix.sh
$ git clone https://github.com/NixOS/nixpkgs.git
$ nix-channel --remove nixpkgs
$ cd ~/.nix-defexpr
$ rm -rf *
$ ln -s /path/of/nixpkgs/clone nixpkgs
```


```
$ sudo mkdir /etc/nix
```

Create a ```/etc/nix/nix.conf``` file with

```
binary-caches = http://cache.nixos.org/ 
```


Update the .bashrc (change your USERNAME):

```
export NIX_PATH=nixpkgs=/Users/USERNAME/dev/nixpkgs
```

Source this
```
$ source ~/.bashrc
```

## Configure Nix for Haskell

Create a ~/.nixpkgs directory and create a ```~/.nixpkgs/config.nix``` file which will be read each time you use nix

```
{
  packageOverrides = super: let self = super.pkgs; in
  {
    haskellEnv = self.haskellngPackages.ghcWithPackages (p: with p; [
      cabal2nix
      cabal-install
    ]);
  };
}
```

Install the haskellEnv with

```
$ nix-env -iA nixpkgs.haskellEnv
```

All of the other dependencies necessary for building your hakyll blog should be in the shell.nix generated by cabal2nix based on what is specified in the cabal file (where you would add/remove dependencies and rerun cabal2nix).

Go to the directory which contains the ```.cabal``` file of your haskell project and use ```cabal2nix``` to help create a ```shell.nix``` environment with the right dependencies.

```
$ cabal2nix --shell . > shell.nix
```

When running this the first time I had an error about the nixpkgs directory and had to source ~/.bashrc with the proper $NIX_PATH variable.

```
$ nix-shell --command 'cabal configure'
```

I got an error message about a nonfree license. Looking more into it, it's my project that's the culprit: the hakyll cabal package isn't declaring a license. Wow, never got a build tool tell me that before. But I guess it makes a whole lot of sense, and it led me to [ponder about](https://github.com/haskell/cabal/issues/847#issuecomment-87065077) what license a website generator should have, is it content or is it code?

I only spent a very short amount of time using cabal, and it seems that with the latest version and ```cabal sandbox``` things improved significantly. Cabal sandboxes allow several version of libraries to co-exist. But the time it took for spinning up a sandbox for a large project is quite long and in the worst case scenario, when dependencies reach into packages that are in the Haskell distribution (like cabal itself) then things start to be quite difficult. What Nix promises is that everything is separated by default, and when it's safe to share packages (same hash of the whole dependency tree) then they won't be rebuilt, meaning less compilation when its not needed! Nix still offers ways to control dependencies in a very granular way (what seems to be called deep overrides).

Time to cabal build:

```
$ cabal build
```

Note that this should be doable from the user environment (i.e. without the nix-shell command) except that it fails with this error: 

```
ghc-7.8.4/include/Stg.h:65:10:
     fatal error: 'math.h' file not found
#include <math.h>
```

Oops, seems like [this thread](https://github.com/NixOS/nixpkgs/issues/6390) says that things don't quite work the way it's supposed to on OSX (because of the clang compiler). The option seems to be to rebuild everything with ```export NIX_CFLAGS_COMPILE="-idirafter /usr/include"```.

Instead I give a shot to running cabal (therefore ghc) from within the nix-shell and tada, it works:

```
$ nix-shell -I ~ --command 'cabal build'
Building iilab-org-hakyll-0.1.0.0...
Preprocessing executable 'site' for iilab-org-hakyll-0.1.0.0...
[1 of 1] Compiling Main             ( site.hs, dist/build/site/site-tmp/Main.o)
Linking dist/build/site/site ...
```

## The workflow

So here it is, I created a simple ```rebuild.sh``` script to rebuild the Hakyll binary and run it with the ```clean``` and ```watch``` arguments:

```
#!/bin/bash
set -e
nix-shell -I ~ --command 'cabal build'
./site clean
./site watch
```

When needed I can also run the repl with 

```
$ nix-shell -I ~ --command 'cabal repl'
```

If I add some Haskell imports and need to update the dependencies, I can do so in the cabal file and regenerate the shell.nix file with ```cabal2nix --shell . > shell.nix```.

## Epilogue

The tooling for Haskell development seems to be evolving fast and progressively making things easier for juggling with dependencies. However the reality is that it is still far from being as simple as other development environments and takes quite a lot of work to setup properly. I also realise that I'm a bit worried about trying to set that up again on another machine if I have to. I do think that the benefits definitely outweigh the drawbacks, and that investing the time to set this up will result in peace of mind when wanting to try out the bleeding edge of Haskell without worrying about breaking other projects because of dependencies. The folks on IRC were very helpful, especially Henry Till (```henrytill``` on IRC) who helped review this post and I recommend heading to the ```##nix-darwin``` IRC Channel on freenode if you run into problems.

I'll share more about why Haskell and Functional Programming matter, as well as my experience with using Hakyll and migrating from metalsmith in a next post but the iilab site is now generated with Hakyll and the [source code is of course available on github](https://github.com/iilab/iilab.org-hakyll).


