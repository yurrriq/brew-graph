module Main where

import Control.Monad (when)
import Data.Char as Char
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Options.Applicative
import System.Exit (ExitCode, exitWith)
import System.Process (rawSystem, runInteractiveCommand, waitForProcess)

-- -------------------------------------------------------------------- [ Main ]

main :: IO ()
main = do opts <- parseOpts
          when (optAll opts) runAll
          when (optInstalled opts) runInstalled
  where
    parseOpts :: IO Opts
    parseOpts = flip customExecParser optsParser $
                prefs $ showHelpOnEmpty <> showHelpOnError

-- -------------------------------------------------------------- [ Data Types ]

data Format = Dot | GraphML
  deriving (Eq, Show)

format :: ReadM Format
format = maybeReader $ go . lowercase
  where
    go :: String -> Maybe Format
    go "dot"     = pure Dot
    go "graphml" = pure GraphML
    go _         = Nothing
    lowercase :: String -> String
    lowercase = map Char.toLower

data Opts = Opts
  { optFormat    :: !Format
  , optOutput    :: !(Maybe FilePath)
  , optAll       :: !Bool
  , optInstalled :: !Bool
  , optArg       :: !(Maybe String)
  }

-- ----------------------------------------------------------------- [ Parsers ]

optsParser :: ParserInfo Opts
optsParser =
    info (helper <*> versionOption <*> programOptions)
         (header "brew-graph - visualize Homebrew dependencies")


formatOption :: Parser Format
formatOption = fmap (fromMaybe Dot) . optional $
               option format $
               short 'f' <> long "format" <>
               metavar "FORMAT" <>
               help "Output format, dot or graphml. Default: dot"

outputOption :: Parser (Maybe FilePath)
outputOption = optional $ strOption $
               short 'o' <> long "output" <>
               metavar "FILE" <>
               help "Write output to FILE instead of stdout"

allFlag :: Parser Bool
allFlag = switch $ long "all" <> help "Graph all Homebrew formulae"

installedFlag :: Parser Bool
installedFlag = switch $
                long "installed" <>
                help "Graph only installed Homebrew formulae"

argOption :: Parser (Maybe String)
argOption = optional $ argument str $
            metavar "FORMULA" <>
            help "Get the dependency graph for FORMULA"

programOptions :: Parser Opts
programOptions = Opts <$>
                 formatOption <*>
                 outputOption <*>
                 allFlag <*>
                 installedFlag <*>
                 argOption

versionOption :: Parser (a -> a)
versionOption = infoOption "0.1.0.0" $
                internal <>
                short 'v' <> long "version" <>
                help "Print version"

-- -------------------------------------------------------- [ Helper Functions ]

runAll :: IO ()
runAll = run $ rawSystem "brew" ["deps", "--all"]

runInstalled :: IO ()
runInstalled = run $ rawSystem "brew" ["deps", "--installed"]

run :: IO ExitCode -> IO ()
run = (exitWith =<<)

-- --------------------------------------------------------------------- [ EOF ]
