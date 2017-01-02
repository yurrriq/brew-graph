{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Data.Homebrew.Graph.Types
Copyright   : (c) Eric Bailey, 2016
License     : BSD-3

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Data types modeling @brew-graph@ command-line options.
-}
module Data.Homebrew.Graph.DOT (
  parseProgDeps,
  parseAllProgDeps,
  renderAllProgDeps
  ) where

import           Control.Monad   (foldM, foldM_)
import           Data.Bifunctor  (bimap)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text       (Text, pack)
import           Text.Dot        (directed, graph, node, renderToStdOut, (-->))

type Prog = Text
type Dep  = Text
type Deps = [Dep]

renderAllProgDeps :: Map Prog Deps -> IO ()
renderAllProgDeps progDeps =
    renderToStdOut $
    graph directed "Homebrew Dependencies" $
    foldM_ bar M.empty (M.toList progDeps)
  where
    bar acc (prog, deps) = do (p, acc') <- upsert prog acc
                              foldM (foo p) acc' deps
    foo p acc dep = do (d, acc') <- upsert dep acc
                       p --> d
                       pure acc'
    upsert k m = case M.lookup k m of
                      Nothing -> do v <- node k
                                    pure (v, M.insert k v m)
                      Just v  -> pure (v, m)

parseProgDeps :: String -> (Prog, Deps)
parseProgDeps = bimap pack (map pack . words . drop 2) . span (/= ':')

parseAllProgDeps :: String -> Map Prog Deps
parseAllProgDeps = M.fromList . map parseProgDeps . lines

-- --------------------------------------------------------------------- [ EOF ]
