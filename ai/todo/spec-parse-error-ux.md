# Spec: Better Parse Error UX

## Problem

`DirectivesParseError` stores the full raw script text in `_directives :: Text`
and its `Show` instance dumps all of it. For any non-trivial script this floods
the terminal, burying the actual megaparsec error message.

A secondary issue: the error thrown from `Main.hs` is an exception printed by
the Haskell runtime (`throwIO`), with no user-friendly framing.

## Goal

When directive parsing fails, print a concise, human-readable error that:
1. Shows only the header lines (those starting with `#!`) where the problem
   lies.
2. Highlights the megaparsec error message without noise.
3. Does not require the user to understand Haskell exception formatting.

## Design

### Trim the stored text

Change `_directives :: Text` in `DirectivesParseError` to store only the
header prefix — lines up to and including the first non-`#!` line, capped at
some small limit (e.g. 20 lines). The full script is not needed for diagnosis.

### Catch and format in Main

Replace the `throwIO` in the parse-failure branch with a direct `logE` +
`exitFailure`:

```haskell
Left e -> do
  logE $ "Failed to parse directives:\n" <> formatDirectivesError e
  exitFailure
```

`formatDirectivesError` renders the trimmed header and megaparsec message
without the wrapping `DirectivesParseError { ... }` noise.

### Example output (after)

```
[ERROR] Failed to parse directives:
  Script header:
    #!/usr/bin/env magix
    #!magix bash
    #!unknown-directive foo

  1:3:
  unexpected "unknown-directive"
  expecting "packages"
```

## Scope

Only directive parse errors are in scope. Build errors already pipe nix-build's
stdout/stderr directly to the logger and are readable.
