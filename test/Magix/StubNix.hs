-- |
-- Module      :  Magix.StubNix
-- Description :  Stubs of the Nix executables
-- Copyright   :  2026 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  not portable
--
-- Creation date: Tue Nov 17 11:03:45 2026.
module Magix.StubNix
  ( StubNix,
    Outcome (..),
    Call (..),
    withStubNix,
    setOutcome,
    setMetadata,
    readCalls,
    getStoreDir,
  )
where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Magix.Tools (withTempDir)
import System.Directory
  ( createDirectory,
    doesFileExist,
    getPermissions,
    removePathForcibly,
    setOwnerExecutable,
    setPermissions,
  )
import System.FilePath ((</>))
import System.Posix.Env (getEnv, setEnv, unsetEnv)

-- | Stubs of @nix-build@ and @nix@, first on @PATH@. @nix flake metadata@
-- prints what 'setMetadata' set.
newtype StubNix = StubNix FilePath

-- | What the following calls of the stubs do.
data Outcome
  = -- | Link the @--out-link@ to a fresh result holding the executable
    -- @bin/<scriptName>@, which prints its arguments, and exit 0.
    Succeed
  | -- | Write to stderr and exit 1.
    Fail
  deriving (Eq, Show)

-- | Set what @nix flake metadata@ prints.
setMetadata :: StubNix -> String -> IO ()
setMetadata (StubNix dir) = writeFile $ dir </> "metadata"

-- | A call of a stub.
data Call = Call
  { -- | The name of the executable.
    program :: !String,
    args :: ![String]
  }
  deriving (Eq, Show)

-- | The script of the stubs. It records its call: the program and each
-- argument on a line, then an empty line.
stubScript :: FilePath -> String -> String
stubScript dir scriptName =
  unlines
    [ "#!/bin/sh",
      "set -e",
      "dir='" <> dir <> "'",
      "{ basename \"$0\"; printf '%s\\n' \"$@\"; echo; } >>\"$dir/calls\"",
      "if [ -e \"$dir/fail\" ]; then",
      "  echo 'error: stub build failed' >&2",
      "  exit 1",
      "fi",
      "if [ \"$1 $2\" = 'flake metadata' ]; then",
      "  cat \"$dir/metadata\"",
      "  exit",
      "fi",
      "out=",
      "while [ $# -gt 0 ]; do",
      "  if [ \"$1\" = --out-link ]; then out=$2; shift; fi",
      "  shift",
      "done",
      "result=$(mktemp -d \"$dir/store/XXXXXX\")",
      "mkdir \"$result/bin\"",
      "exe=\"$result/bin/" <> scriptName <> "\"",
      "printf '#!/bin/sh\\necho \"$@\"\\n' >\"$exe\"",
      "chmod +x \"$exe\"",
      -- Like Nix, replace an existing out-link.
      "ln -sfn \"$result\" \"$out\""
    ]

-- | Run the action with stubs of @nix-build@ and @nix@ first on @PATH@,
-- building results with the executable @bin/<scriptName>@; they succeed until
-- told otherwise. The stubs and their results live in a temporary directory,
-- removed afterwards. @PATH@ is global to the process: run no tests in
-- parallel to this.
withStubNix :: String -> (StubNix -> IO a) -> IO a
withStubNix scriptName action = withTempDir $ \dir -> do
  let binDir = dir </> "bin"
  createDirectory binDir
  createDirectory $ dir </> "store"
  forM_ ["nix-build", "nix"] $ \name -> do
    let path = binDir </> name
    writeFile path $ stubScript dir scriptName
    getPermissions path >>= setPermissions path . setOwnerExecutable True
  bracket (getEnv "PATH") restore $ \path -> do
    setEnv "PATH" (binDir <> maybe "" (':' :) path) True
    action $ StubNix dir
  where
    restore = maybe (unsetEnv "PATH") (\p -> setEnv "PATH" p True)

setOutcome :: StubNix -> Outcome -> IO ()
setOutcome (StubNix dir) = \case
  Succeed -> removePathForcibly failFile
  Fail -> writeFile failFile ""
  where
    failFile = dir </> "fail"

-- | The calls of the stubs, in order.
readCalls :: StubNix -> IO [Call]
readCalls (StubNix dir) = do
  let path = dir </> "calls"
  exists <- doesFileExist path
  if exists then parse . lines <$> readFile path else pure []
  where
    parse ls = case break null ls of
      ([], []) -> []
      (p : as, rest) -> Call p as : parse (drop 1 rest)
      ([], rest) -> parse (drop 1 rest)

-- | The directory holding the results of the stubs; canonical.
getStoreDir :: StubNix -> FilePath
getStoreDir (StubNix dir) = dir </> "store"
