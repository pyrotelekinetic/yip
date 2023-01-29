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

{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Void (Void)
import Data.Text as T
import Data.Text.IO as T
import Text.Megaparsec
import Text.Megaparsec.Char (newline, space, string)

type Parser = Parsec Void Text

data Line = Insert FilePath | Literal Text
  deriving Show

parseFile :: Parser [Line]
parseFile = (Insert <$> try parsePath <|> Literal <$> parseLine) `sepEndBy` newline

parsePath :: Parser FilePath
parsePath = do
  space
  string "###"
  r <- some $ notFollowedBy (string "###") *> noneOf ['\n', '\0']
  string "###"
  space
  pure r

parseLine :: Parser Text
parseLine = pack <$> some (anySingleBut '\n')
