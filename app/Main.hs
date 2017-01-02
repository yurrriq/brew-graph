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

import           Data.Homebrew.Graph

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import           Control.Exception       (finally)
import           Control.Monad           (unless, when)
import           Data.Map.Strict         (Map)
import           System.Exit             (ExitCode (..), exitWith)
import           System.IO               (hGetContents, hPutStr, hPutStrLn,
                                          stderr)
import           System.Process          (runInteractiveCommand, waitForProcess)

-- -------------------------------------------------------------------- [ Main ]

main :: IO ()
main = do opts <- parseOpts
          when (optAll opts) $
               brewDeps "--all"
          when (optInstalled opts) $
               brewDeps "--installed"
  where
    brewDeps flags = runCommand' (unwords ["brew deps",flags]) >>=
                     renderAllProgDeps . parseAllProgDeps

-- -------------------------------------------------------- [ Running Commands ]

-- | @'runCommand'' cmd = 'runCommand' cmd []@
runCommand' :: String -> IO String
runCommand' cmd = runCommand cmd []

-- Modified from the version in Run.hs, part of the Annote project,
-- which is licensed under The 3-Clause BSD License.
--
-- http://www.math.columbia.edu/~bayer/Haskell/Annote/Run.html
--
runCommand :: String -> String -> IO String
runCommand cmd input =
    do (inp,out,err,pid) <- runInteractiveCommand cmd
       let get h = do mvar <- newEmptyMVar
                      let put xs = seq (length xs) (putMVar mvar xs)
                      forkIO $ finally (hGetContents h >>= put) (put [])
                      takeMVar mvar
       unless (null input) $ hPutStr inp input
       output <- get out
       errMsg <- get err
       exit   <- waitForProcess pid
       case exit of
            ExitSuccess -> pure output
            code        -> do hPutStrLn stderr errMsg
                              exitWith code

-- --------------------------------------------------------------------- [ EOF ]
