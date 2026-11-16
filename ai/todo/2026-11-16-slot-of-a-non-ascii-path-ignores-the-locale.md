# The slot of a non-ASCII path ignores the locale

GHC decodes a path with the locale's file system encoding: under `C.UTF-8`,
`ä` is one `Char`; under `C`, it is two escape characters (U+DCC3 U+DCA4).
`getSlotHash` (`src/Magix/Hash.hs`) encodes the decoded `String` with
`charUtf8`, so the same path gets another slot hash under each locale. The slot
name repeats it: `replaceProblematicChars` (`src/Magix/Paths.hs`) keeps `ä`
and turns each escape character into `-`.

On 2026-11-16, one script at `…/dä/script`, run under `C` and then under
`C.UTF-8` into one cache, got two slots, and the second run rebuilt. Both
builds were the same store path, so it costs a rebuild and a slot, not store
space; `--stale` keeps both, since the path exists.

Hash the path's bytes instead: encode the `FilePath` back with the file system
encoding (`GHC.Foreign`), or move to `OsPath`. Derive the slot name from the
same bytes. A unit test hashes `ä` and its escaped form, each under its
encoding, and expects one slot.

`buildModeBytes` encodes a channel path the same way; a non-ASCII nixpkgs path
gets another content hash under each locale.
