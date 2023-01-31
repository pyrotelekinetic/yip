{-
 -  yip - A very simple pre processor
 -  Copyright (C) 2023  Carter "pyrotelekinetic" Ison <carter@isons.org>
 -
 -  This program is free software: you can redistribute it and/or modify
 -  it under the terms of the GNU Affero General Public License as published by
 -  the Free Software Foundation, either version 3 of the License, or
 -  (at your option) any later version.
 -
 -  This program is distributed in the hope that it will be useful,
 -  but WITHOUT ANY WARRANTY; without even the implied warranty of
 -  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 -  GNU Affero General Public License for more details.
 -
 -  You should have received a copy of the GNU Affero General Public License
 -  along with this program.  If not, see <https://www.gnu.org/licenses/>.
 -}

module Command (parseOpts, Opts (..)) where

import Options.Applicative

data Opts = Opts
  { file :: FilePath
  }

parseOpts :: IO Opts
parseOpts = execParser . info parser $ fullDesc
  <> progDesc "insert file contents into file"

parser :: Parser Opts
parser = Opts <$> fileP <**> helper

fileP :: Parser FilePath
fileP = strArgument
  $ metavar "FILE"
  <> help "The file to process"