module Command (parseOpts, Opts (..)) where

import Options.Applicative

data Opts = Opts
  { input :: FilePath
  , output :: FilePath
  , noRecurse :: Bool
  } deriving Show

parseOpts :: IO Opts
parseOpts = execParser . info parser $ fullDesc
  <> progDesc "insert file contents into file"

parser :: Parser Opts
parser = Opts <$> inputP <*> outputP <*> recurseP <**> helper

inputP :: Parser FilePath
inputP = strArgument
  $ metavar "FILE"
  <> help "The file to process"

outputP :: Parser FilePath
outputP = strOption
  $ metavar "OUTPUT"
  <> help "Write output to OUTPUT"
  <> short 'o'
  <> long "output"
  <> value "-"

recurseP :: Parser Bool
recurseP = switch
  $ help "Disable recursive file processing"
  <> short 'R'
  <> long "no-recurse"
