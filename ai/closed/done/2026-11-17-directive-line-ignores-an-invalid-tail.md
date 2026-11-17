# A directive line ignores an invalid tail

From the first character outside the grammar, the rest of a directive line
becomes script body instead of failing the parse. Found by the QuickCheck
properties of `pDirectives` (`test/Magix/DirectivesPropertiesSpec.hs`), whose
property "rejects an invalid character anywhere in a value" is `pending` on
this note. Confirmed in GHCi on 2026-11-17:

- `#!packages _ \DELP` parses as packages `["_"]` (shrunk counterexample).
- `#!packages jq\DELfoo` parses as `["jq"]`; the body starts `\DELfoo`.
- `#!magix bashful` parses as `bash`, with body `ful…`. A language whose name
  is a prefix of another's (`r`, `ruby`) would hit this.
- A CRLF script with one directive is accepted, with `\r` left in the body;
  with two directives it is rejected.

Cause: `pManyDirectives` and `pLanguageDirectives` don't require a directive
line to end at a newline or the end of input.

Fix: require `hspace *> (void newline <|> eof)` after each directive line,
the `#!magix` line included, then un-pend the property. Open: should a CRLF
line ending be accepted or rejected with a clear message?

## Outcome

Fixed on 2026-11-17 (`fix(directives): end a directive line at a newline or
the end of input`). `pLineEnd` (`src/Magix/Language/Common/Directives.hs`)
requires inline white space, then a newline or the end of input, after every
directive line: the shebang, `#!magix`, `#!nixpkgs` and each language
directive. A carriage return before the line end fails with a message naming
CRLF line endings: decided, a CRLF script is rejected, as its shebang cannot
run on Linux anyway. The property is un-pended, and unit tests cover the four
cases above; all fail without the fix. `pLanguage` still picks the first
language whose name prefixes the input; with the line end required, a prefix
clash such as `r` and `ruby` now fails instead of parsing as the shorter one.
