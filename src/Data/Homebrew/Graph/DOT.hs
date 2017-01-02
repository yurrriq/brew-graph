{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Data.Homebrew.Graph.Types
Copyright   : (c) Eric Bailey, 2016
License     : BSD-3

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Generating directed graphs in the
<http://www.graphviz.org/content/dot-language GraphViz DOT> format.
-}
module Data.Homebrew.Graph.DOT (
  -- * Type Aliases
  Prog, Deps,
  -- * API
  renderAllProgDeps, parseAllProgDeps, parseProgDeps
  ) where

import           Control.Monad   (foldM, foldM_)
import           Data.Bifunctor  (bimap)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text       (Text, pack)
import           Text.Dot        (directed, graph, node, renderToStdOut, (-->))

-- | A program's name.
type Prog = Text

-- | A list of dependencies.
type Deps = [Prog]

-- | Render a map from programs to their dependencies as a directed graph in
-- <http://www.graphviz.org/content/dot-language GraphViz DOT> format and print
-- it to stdout.
renderAllProgDeps :: Map Prog Deps -> IO ()
renderAllProgDeps progDeps =
    renderToStdOut $
    graph directed "Homebrew Dependencies" $
    foldM_ outer M.empty (M.toList progDeps)
  where
    outer acc (prog, deps) = do (p, acc') <- upsert prog acc
                                foldM (inner p) acc' deps
    inner p acc dep = do (d, acc') <- upsert dep acc
                         p --> d
                         pure acc'
    upsert k m = case M.lookup k m of
                      Nothing -> do v <- node k
                                    pure (v, M.insert k v m)
                      Just v  -> pure (v, m)

-- | Parse a newline-separated list of strings of the form @"prog: deps ..."@
-- into a map from programs to their dependencies.
--
-- @'parseAllProgDeps' = M.fromList . map 'parseProgDeps' . lines@
parseAllProgDeps :: String -> Map Prog Deps
parseAllProgDeps = M.fromList . map parseProgDeps . lines

-- | Parse a string of the form @"prog: deps ..."@ into a pair of a program and
-- its dependencies.
--
-- @'parseProgDeps' "erlang: jpeg libpng libtiff openssl wxmac" ==
--   ("erlang",["jpeg","libpng","libtiff","openssl","wxmac"])@
parseProgDeps :: String -> (Prog, Deps)
parseProgDeps = bimap pack (map pack . words . drop 2) . span (/= ':')

-- --------------------------------------------------------------------- [ EOF ]
