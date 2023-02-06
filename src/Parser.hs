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

module Parser (parse, Chunk(..)) where

import Data.Void (Void)
import Data.Maybe (fromJust, maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char (newline, hspace, eol)
import Data.Char (isSpace)
import Data.Functor (void)
import Control.Applicative (liftA2)

type Parser = Parsec Void Text

data Chunk
  = Literal Text
  | Replace Text
  | Insert FilePath (Maybe [(Text, Text)])
  | Newline
  deriving Show

sepEndBy' :: Parser a -> Parser a -> Parser [a]
sepEndBy' p sep = liftA2 (:) p (liftA2 (:) sep (sepEndBy' p sep) <|> pure []) <|> pure []

replace :: Parser Chunk
replace = do
  chunk "{!"
  r <- some $ satisfy (\x -> not (isSpace x) && x /= '!')
  chunk "!}"
  pure . Replace $ T.pack r

insert :: Parser Chunk
insert = do
  hspace
  chunk "{:"
  r <- some $ notFollowedBy (chunk ":}") *> noneOf ['\n', '\0']
  chunk ":}"
  hspace
  pure $ Insert r Nothing

literal :: Parser Chunk
literal = do
  xs <- some $ do
    notFollowedBy (void replace <|> void eol <|> eof)
    anySingle
  pure . Literal . T.pack $ xs

file :: Parser [Chunk]
file = do
  r <- concat <$> sepEndBy' (pure <$> try insert <|> many (try replace <|> literal)) ([Newline] <$ eol)
  eof
  pure r

parse :: Text -> [Chunk]
parse a = fromJust $ parseMaybe file a
