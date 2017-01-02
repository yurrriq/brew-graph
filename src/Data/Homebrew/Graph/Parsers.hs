{-|
Module      : Data.Homebrew.Graph.Parsers
Copyright   : (c) Eric Bailey, 2016
License     : BSD-3

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Parsers for @brew-graph@ command-line options.
-}
module Data.Homebrew.Graph.Parsers (
  -- * API
  parseOpts,
  -- * Parsers
  optsParser,
  -- * Flags
  allFlag, installedFlag,
  -- * Options
  argOption, formatOption, outputOption, programOptions -- , versionOption
  ) where

import Data.Homebrew.Graph.Types

import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))

import Options.Applicative

-- --------------------------------------------------------------------- [ API ]

-- | Parse all @brew-graph@ command-line options.
parseOpts :: IO Opts
parseOpts = flip customExecParser optsParser $
                prefs $ showHelpOnEmpty <> showHelpOnError

-- ----------------------------------------------------------------- [ Parsers ]

-- | The full description of a runnable 'Parser' for @brew-graph@.
optsParser :: ParserInfo Opts
optsParser =
    info (helper <*> versionOption <*> programOptions)
         (header "brew-graph - visualize Homebrew dependencies")

-- ------------------------------------------------------------------- [ Flags ]

-- | Graph all Homebrew formulae. See also: 'installedFlag'.
allFlag :: Parser Bool
allFlag = switch $ long "all" <> help "Graph all Homebrew formulae"

-- | Graph only installed Homebrew formulae. See also: 'allFlag'.
installedFlag :: Parser Bool
installedFlag = switch $
                long "installed" <>
                help "Graph only installed Homebrew formulae"

-- ----------------------------------------------------------------- [ Options ]

-- | Get the dependency graph for a given formula.
argOption :: Parser (Maybe String)
argOption = optional $ argument str $
            metavar "FORMULA" <>
            help "Get the dependency graph for FORMULA"

-- | Case-insensitive output format, 'DOT' or 'GraphML'. Default: 'DOT'.
formatOption :: Parser Format
formatOption =
    fmap (fromMaybe DOT) . optional $
    option format $
    short 'f' <> long "format" <>
    metavar "FORMAT" <>
    help "Case-insensitive output format, DOT or GraphML. Default: dot"

-- | Write output to a given file, instead of stdout.
outputOption :: Parser (Maybe FilePath)
outputOption = optional $ strOption $
               short 'o' <> long "output" <>
               metavar "FILE" <>
               help "Write output to FILE instead of stdout"

-- | Parse all command-line options.
programOptions :: Parser Opts
programOptions = Opts <$>
                 formatOption <*>
                 outputOption <*>
                 allFlag <*>
                 installedFlag <*>
                 argOption

-- | Print the version and exit.
versionOption :: Parser (a -> a)
versionOption = infoOption "0.1.0.0" $
                internal <>
                short 'v' <> long "version" <>
                help "Print the version and exit"

-- --------------------------------------------------------------------- [ EOF ]
