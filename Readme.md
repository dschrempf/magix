[Changelog](Changelog.md).

# Magix

Build, cache, and run possibly compiled scripts with dependencies using the [Nix
package manager](https://nixos.org/).

- Magix is simple and stupid.
- Magix is a tiny wrapper around `nix-build`.
- Magix uses Nix expression templates, and so, is easy to understand, modify,
  and enhance.
- Magix is heavily tested (only unit tests at the moment, but please drop a pull
  request).

# Supported languages

## Bash

``` bash
#!/usr/bin/env magix
#!magix bash
#!packages jq

jq --help
```

[Bash Nix expression template](src/Magix/Language/Bash/Template.nix).

## Haskell

``` haskell
#!/usr/bin/env magix
#!magix haskell
#!ghcFlags -threaded
#!haskellPackages bytestring

{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString qualified as BS

main :: IO ()
main = BS.putStr "Hello, World!\n"
```

[Haskell Nix expression template](src/Magix/Language/Haskell/Template.nix).

## Python

``` python
#!/usr/bin/env magix
#!magix python
#!pythonPackages numpy

from numpy import array

xs = array([1,2,3])
print(xs)
```

[Python Nix expression template](src/Magix/Language/Python/Template.nix).

# Try

Try Magix without installation on a Bash script called
[`args`](test-scripts/bash/args)

``` bash
wget https://github.com/dschrempf/magix/raw/refs/heads/main/test-scripts/bash/args
nix run github:dschrempf/magix#magix -- args one two three -h
```

``` text
Command basename is: args
Command line arguments are: one two three -h
```

# Get help

``` bash
magix -h
```

``` text
Magix version 1.0.0.0

Usage: magix [--version]
             (COMMAND | [-v|--verbose] [-f|--force-build]
               [-c|--cache-path CACHE_PATH] [-n|--nixpkgs-path NIXPKGS_PATH]
               [-r|--nixpkgs-ref NIXPKGS_REF] SCRIPT_FILE_PATH [SCRIPT_ARGS])

  Build, cache, and run possibly compiled scripts with dependencies using the
  Nix package manager

Available options:
  --version                Show version
  -h,--help                Show this help text
  -v,--verbose             Print debug messages
  -f,--force-build         Force build, even when cached build exists
  -c,--cache-path CACHE_PATH
                           Path of cache directory to use for builds (default:
                           '$XDG_CACHE_HOME/magix')
  -n,--nixpkgs-path NIXPKGS_PATH
                           Path of Nixpkgs to use (default: found in
                           '$NIX_PATH', see below)
  -r,--nixpkgs-ref NIXPKGS_REF
                           Nixpkgs Flake reference, e.g. 'nixpkgs' or
                           'github:NixOS/nixpkgs/nixos-unstable'; when set, uses
                           'nix build' instead of 'nix-build'
  SCRIPT_FILE_PATH         File path of script to build, cache and run
  SCRIPT_ARGS              Arguments passed on to the script

Available commands:
  clean-magix-cache        Clean the Magix cache.

Magix-specific environment variables:
  MAGIX_NIXPKGS_REF
    Nixpkgs Flake reference; overridden by '--nixpkgs-ref'.
    When set, uses 'nix build' instead of 'nix-build'.
Nix environment variables:
  NIX_PATH
    Used to find Nixpkgs when neither '--nixpkgs-path' nor 'MAGIX_NIXPKGS_REF'
    is set: the first entry providing it, 'nixpkgs=<path>' or an unprefixed
    directory '<dir>' holding '<dir>/nixpkgs'.
    A 'flake:<id>' target resolves through the user and the system flake
    registry to the path of a 'path' entry. magix does not read the global
    registry, and fails on other targets, such as 'channel:' and URL targets.
  NIX_CONFIG_HOME
    Directory of the user flake registry (default: '$XDG_CONFIG_HOME/nix', else
    '$HOME/.config/nix').
  XDG_CONFIG_HOME
    See 'NIX_CONFIG_HOME'.
  NIX_CONF_DIR
    Directory of the system flake registry (default: '/etc/nix').
```

# New languages

We have designed Magix so that implementing new languages is straightforward. In
particular:

- Change and add modules only within the `src/Magix/Language/*` namespace.
- Add language-specific tests to `test/Magix/Language/*`.

# Next steps

- Property-based testing (e.g., generate arbitrary directives or even scripts).
  When creating arbitrary directives, one could test if the resulting Nix
  expressions are syntactically correct.
- We create random caches and hashes during tests. We could write an Arbitrary
  instance for `Config` to simplify this process.

# Notes about performance

I have performed basic benchmarks and have recorded some profiles. When a script
is cached, **Magix has a runtime cost of around 20ms**.

# Similar tools that I know of

- [bennofs/nix-script](https://github.com/bennofs/nix-script): Does not
  pre-compile scripts, does not cache compilations; however, Haskell code base
  and complexity seems to be much smaller.
- [BrianHicks/nix-script](https://github.com/BrianHicks/nix-script): Magix was
  heavily inspired by
  [BrianHicks/nix-script](https://github.com/BrianHicks/nix-script), which [I
  also maintain](https://github.com/dschrempf/nix-script). However, my Rust is a
  bit rusty, and I wanted a simpler solution.

I want `magix` to be a **simple and fast solution** that just works. If you are
looking for a wrapper with more extras, try the [Nix package manager
;-)](https://github.com/NixOS/nix).
