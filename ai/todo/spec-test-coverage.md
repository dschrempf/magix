# Spec: Close Test Coverage Gaps

## Problem

Two TODOs in `test/Magix/DirectivesSpec.hs` note that certain tests are written
manually for specific languages rather than being data-driven over all languages:

1. **Line 85** — the "fails on wrong language directives" block hardcodes `bash`
   in the failure inputs (`#!magix bash\n\n#!packages bar` etc.). Adding a new
   language does not automatically exercise these failure paths.

2. **Line 100** — the "parses X language directives" success cases are written
   out individually per language. Again, adding a language requires remembering
   to add a test here.

The concern is not test duplication today, but that the pattern makes it easy
to forget to update tests when adding a language.

## Goal

Make the `pLanguageDirectives` tests data-driven so they automatically cover
every `Language` constructor, matching the existing pattern used by
`pEmptyLanguageDirectivesFor`.

## Design

### Failure tests

Generalise the hardcoded-bash failure cases into a helper
`pLanguageDirectivesFailFor :: Language -> SpecWith ()` that constructs failure
inputs using `getLanguageNameLowercase`, then call it with `mapM_ ... languages`.

The existing mixed failure cases that are not language-specific (e.g.
`"#! bar"`, `"#!magix unknown"`) stay as a shared `it` block.

### Success tests

Introduce `getMinimalDirectiveInput :: Language -> Text` in
`Magix.Languages.Tools` alongside the existing `getMinimalTestcase`. This
returns a one-liner directive string for each language (e.g.
`"#!magix bash\n#!packages bar"`). Then drive the success spec with
`mapM_ pMinimalDirectivesFor languages`.

### Scope

Only `DirectivesSpec.hs` and `Magix.Languages.Tools` are in scope. Expression
specs are already data-driven via `testExpression`.

## Expected outcome

- The two TODO comments are removed.
- Adding a new language to `Language` automatically includes it in all
  `pLanguageDirectives` tests without any manual additions to the test file.
