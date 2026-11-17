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
