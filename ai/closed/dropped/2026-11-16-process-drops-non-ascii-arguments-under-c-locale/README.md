# Report `process` dropping non-ASCII arguments under the C locale

Dropped, not filed: "Posix: treat arguments using filesystem encoding"
(haskell/process#388), opened 2026-11-15, reports the bug and fixes it.
`report.md` holds the draft.

The draft calls `withCString` an oversight. It was deliberate: "Posix: Do not
treat arguments as `FilePath`" (haskell/process#320, process 1.6.22.0) made the
switch so that a NUL byte in an argument no longer throws ("NUL byte disallowed
in arguments", haskell/process#319).
