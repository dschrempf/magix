# `#!nixpkgs` parses only right after the shebang

Date: 2026-11-18

`pDirectives` (`src/Magix/Directives.hs`) reads the shebang, an optional
`#!nixpkgs`, then `#!magix` and the language's directives, followed by no
other `#!` line. A `#!nixpkgs` anywhere else fails, with an error that does
not name it:

```
#!/usr/bin/env magix
#!magix bash
#!packages hello
#!nixpkgs nixpkgs
```

```
4:3:
unexpected "nixpkgs "
expecting "packages"
```

The Readme names `#!nixpkgs` only beside `lib.build`, and nowhere says where it
goes; the test-scripts `hello-flake` show the one order that parses.

Likely a bug, not a design: nothing depends on the position. Either:

- accept `#!nixpkgs` once anywhere among the directives after the shebang
  (recommended), or
- keep the order, document it under the usage of the Readme, and name
  `nixpkgs` in the error: `expecting "packages"` after `#!magix bash` hides
  the cause.

Related: `parse-error-shows-the-header-alone`.

## Outcome

Fixed on branch `fix/directives-in-any-order`, beyond the recommendation: all
directives after the shebang come in any order. `pDirectives` looks ahead for
`#!magix`, then parses each directive line with the Magix, the Nixpkgs, or the
language's one-line parser. A second `#!magix` or `#!nixpkgs`, and a missing
`#!magix`, fail naming it; an unknown directive fails listing the known ones.
The Readme documents the directives in a section of their own.
