{-|
Module      : Data.Homebrew.Graph.Types
Copyright   : (c) Eric Bailey, 2016
License     : BSD-3

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Data types modeling @brew-graph@ command-line options.
-}
module Data.Homebrew.Graph.Types (
  -- * Data Types
  Format(..), Opts(..),
  -- * Readers
  format
  ) where

import Data.Char as Char
import Options.Applicative (ReadM, maybeReader)

-- -------------------------------------------------------------- [ Data Types ]

-- | Supported output formats are 'DOT' and 'GraphML'.
data Format =
    DOT         -- ^ <http://www.graphviz.org/content/dot-language GraphViz DOT>
  | GraphML     -- ^ <http://graphml.graphdrawing.org GraphML>


-- | Command-line options.
data Opts = Opts
  { optFormat    :: !Format           -- ^ Output 'Format'. Default: 'DOT'.
  , optOutput    :: !(Maybe FilePath) -- ^ Output filename.
  , optAll       :: !Bool             -- ^ All formulae.
  , optInstalled :: !Bool             -- ^ Only installed formulae.
  , optArg       :: !(Maybe String)   -- ^ A specific formula.
  }

-- ----------------------------------------------------------------- [ Readers ]

-- | A 'Format' reader.
format :: ReadM Format
format = maybeReader (maybeFormat . lowercase)

-- ------------------------------------------------------ [ Internal Functions ]

-- | Given a lowercase 'String', attempt to coerce to 'Format'.
maybeFormat :: String -> Maybe Format
maybeFormat "dot"     = pure DOT
maybeFormat "graphml" = pure GraphML
maybeFormat _         = Nothing

-- | Convert a given 'String' to lowercase.
lowercase :: String -> String
lowercase = map Char.toLower

-- --------------------------------------------------------------------- [ EOF ]
