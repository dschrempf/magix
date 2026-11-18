# Locking a branch skips Nix within `tarball-ttl`

A `#!nixpkgs` reference neither pinned nor resolved to a store path through
the flake registries, such as `github:<owner>/<repo>`, costs a call of
`nix flake metadata` on every run: about 70 ms, against 5.5 ms for a cached
run (measured 2026-11-18). Nix itself asks the forge again only once per
`tarball-ttl` (default 1 h).

magix could cache Nix's answer for a remote reference for `tarball-ttl`,
the staleness Nix already accepts. A `path:` or `git+file:` reference
would still ask Nix on every run, since an edit shows at once. It adds a
cache entry that `clean-magix-cache` has to know.

Deferred 2026-11-18 in a-cached-script-ignores-changes-to-its-nixpkgs-flake
(#27): the 70 ms were accepted; revisit if they bother in practice.
