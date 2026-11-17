# A run logs errors alone by default

Without `-v`, magix logs at `INFO` (`setupLogger`, `app/Main.hs`;
`pLogLevel`, `src/Magix/Options.hs`), so every call of a cached script
writes two lines to stderr: "Script has already been built" and "Build
complete, executing script: …". A script that stands in for a program, such
as `nmap` or `7za` installed as a magix script, then prints them on every
use, and a shebang cannot pass a flag to quiet them. Found on 2026-11-17,
when a system configuration installed programs of nixpkgs as magix scripts.

Log at `ERROR` by default; `-v` keeps `DEBUG`. The `INFO` messages of a run
(`src/Magix/Main.hs`) move to `DEBUG`, or a level between them gets a flag of
its own.

Open:

- The first call of a script may build for minutes, and `nix-build`'s output
  is captured (`src/Magix/Build.hs`), shown only on failure or with `-v`.
  Today "Need to build" is the one sign of it; keep a line like it, so the
  silence is not mistaken for a hang.
- `clean-magix-cache` logs at `INFO` too ("Cleaning Magix cache", "Done"):
  a command run by hand may keep them, a timer does not need them.
