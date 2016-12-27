{-|
Module      : Data.Homebrew.Graph
Copyright   : (c) Eric Bailey, 2016
License     : BSD-3

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

TODO: write docstring
-}
module Main (main) where

import Data.Homebrew.Graph

import Control.Monad (when)
import System.Exit (ExitCode, exitWith)
import System.Process (rawSystem)

-- -------------------------------------------------------------------- [ Main ]

main :: IO ()
main = do opts <- parseOpts
          when (optAll opts) runAll
          when (optInstalled opts) runInstalled

-- -------------------------------------------------------- [ Helper Functions ]

-- | @brew deps --all@
runAll :: IO ()
runAll = run $ rawSystem "brew" ["deps", "--all"]

-- | @brew deps --installed@
runInstalled :: IO ()
runInstalled = run $ rawSystem "brew" ["deps", "--installed"]

run :: IO ExitCode -> IO ()
run = (exitWith =<<)

-- --------------------------------------------------------------------- [ EOF ]
