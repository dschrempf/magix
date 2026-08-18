# Spec: Additional Language Support

## Goal

Extend Magix with support for more scripting languages. The architecture already
provides a clear recipe (`CLAUDE.md` § "Adding a New Language"); this spec
identifies the best candidates and notes per-language considerations.

## Candidate languages

### Ruby

- Directive: `#!magix ruby` / `#!rubyGems <gem> ...`
- Template: `pkgs.stdenv.mkDerivation` with a `ruby.withPackages` env, similar
  to Python. Ruby scripts keep their original shebang replaced by the wrapped
  interpreter.
- Consideration: `ruby.withPackages` in nixpkgs has limited gem coverage; gems
  not in nixpkgs need `bundlerEnv`, which is significantly more complex.
  Restrict initial support to nixpkgs-packaged gems only.

### JavaScript (Node.js)

- Directive: `#!magix node` / `#!nodePackages <pkg> ...`
- Template: `pkgs.writeScriptBin` wrapping `node` with `NODE_PATH` set to a
  `nodePackages`-derived env.
- Consideration: the nixpkgs `nodePackages` set is auto-generated and sometimes
  lags behind npm; package names differ from npm (e.g. `pkgs.nodePackages."@babel/core"`).

### Lua

- Directive: `#!magix lua` / `#!luaPackages <pkg> ...`
- Template: `pkgs.lua.withPackages` — straightforward, mirrors Python.
- Consideration: Lua has two common flavours in nixpkgs (`lua` vs `luajit`);
  may need a `#!luaInterpreter luajit` sub-directive.

### R

- Directive: `#!magix r` / `#!rPackages <pkg> ...`
- Template: `pkgs.rWrapper.override { packages = with pkgs.rPackages; [...]; }`
- Consideration: R scripts are not executable files in the traditional sense;
  the template must produce a wrapper shell script that calls `Rscript`.

## Implementation order

Ruby and Node.js have the broadest use cases. Lua and R are niche but
mechanically simple. Each language is independent and can be implemented
separately.

## Testing

Each new language needs:
- `test-scripts/<lang>/minimal` and `test-scripts/<lang>/args`
- `test/Magix/Languages/<Lang>/DirectivesSpec.hs`
- `test/Magix/Languages/<Lang>/ExpressionSpec.hs`
- An entry in `Magix.Languages.Tools` (`getEmptyDirectives`, `getMinimalTestcase`)
