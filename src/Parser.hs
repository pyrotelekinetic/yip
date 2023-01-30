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

module Parser (parse, Line(..)) where

import Data.Void (Void)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char (space, string)

type Parser = Parsec Void Text

data Line = Insert FilePath | Literal Text
  deriving Show

parse :: Text -> [Line]
parse a = fromJust $ parseMaybe file a

file :: Parser [Line]
file = do
  r <- many $ (Insert <$> try path) <|> Literal <$> line
  eof
  pure r

path :: Parser FilePath
path = do
  string "###"
  r <- some $ notFollowedBy (string "###") *> noneOf ['\n', '\0']
  string "###"
  space
  pure r

line :: Parser Text
line = do
  l <- some (anySingleBut '\n')
  space
  pure $ T.pack l
