{-
Copyright (c) 2026, MiniGHC contributors
All rights reserved. See LICENSE.
-}
module Main (main) where

import qualified MiniGHC
import MiniGHC.Lint (lintFile, formatLint, lintErrors)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--lint", path] -> do
      res <- lintFile path
      putStr (formatLint path res)
      if null (lintErrors res) then exitSuccess else exitFailure
    ("--lint":_) -> do
      hPutStrLn stderr "Usage: minighc --lint FILE.mgh"
      exitFailure
    _ -> MiniGHC.main
