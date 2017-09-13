---
title: Cabal Notes
published: July 23, 2017
---

Notes on the Cabal install tool.

<!--more-->

# Getting Started with Cabal

## Installing

1. Update `apt-get`: `sudo apt-get update`
2. Install the haskell platform, has ghc, ghci, cabal, etc: `sudo apt-get install haskell-platform`

## Cabal Sandbox

1. Make a directory.
2. Move into directory.
3. `cabal sandbox init`: this makes the sandbox files. All cabal commands will now only operate in the sandbox.

## Cabal Setup

1. `cabal init`: this sets up the package and makes a `.cabal` file.
2. Make `.hs` file that is the launch point of the package (this will get imported when you run `cabal repl`)
3. In the `.cabal` file define some exposed module. This is important, needs to exist along with its corresponding `.hs` file, otherwise `cabal repl` won't work.
4. Install packages, this can either happen with:
    * `cabal install -j <package name>`, or
    * if you've added other packages to the `build-depends` list in the `.cabal` file you can run: `cabal install -j --only-dependencies`.
    * The `-j` parallizes installation, by default it uses the number of cpus, you can optionally specify a number.
5. You can run `cabal repl` now, this will drop you into ghci with the exposed module already loaded.

## Other Cabal-abilites

* Binaries that are installed by cabal are available via: `cabal exec <name_of_binary>`. e.g. `cabal exec hakyll-init .`
* After `cabal build`ing, if the target was an executable (this would be specified in the `.cabal` file), you can run the binaries with `cabal run <name_of_execuatble>`. The name of this executable matches the one specified in the `.cabal` file.
 
 ```
 cabal run site watch
 ```
* It looks like under the hood, when you run `cabal build`, cabal is doing all of the ghc calls for you while making sure that packages are available. Once the program is compiled it puts the executable in the `dist` folder. Running `cabal run <name_of_execuatble>` tells cabal to go find the executable by name and run it.

* When you want to pass options, flags, to programs that are run with `cabal run` use a `--` to separate them. e.g.

 ```
 cabal run site watch -- --host 0.0.0.0
 ```
