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

module Parser (parse, Chunk(..), Replacements) where

import Data.Void (Void)
import Data.Maybe (fromJust, maybeToList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char (newline, hspace, hspace1, eol)
import Text.Megaparsec.Char.Lexer (charLiteral)
import Data.Char (isSpace)
import Data.Functor (void)
import Control.Applicative (liftA2)

type Parser = Parsec Void Text

type Replacements = Map Text Text

data Chunk
  = Literal Text
  | Replace Text
  | Insert FilePath Replacements
  | Newline
  deriving Show

sepEndBy' :: Parser a -> Parser a -> Parser [a]
sepEndBy' p sep = liftA2 (:) p (liftA2 (:) sep (sepEndBy' p sep) <|> pure []) <|> pure []

path :: Parser Text -> Parser FilePath
path p = f <|> some
  ( do
    notFollowedBy $ void p <|> hspace1
    noneOf ['\n', '\0']
  )
  where
  f :: Parser FilePath
  f = f' (single '"') <|> f' (single '\'')
    where
    f' :: Parser a -> Parser String
    f' p = p >> manyTill charLiteral p

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
  hspace
  r <- path $ chunk ":}"
  a <- M.fromList <$> many p
  chunk ":}"
  hspace
  pure $ Insert r a
  where
  p :: Parser (Text, Text)
  p = do
    hspace
    r <- some $ noneOf ['\n', '\0', ':']
    single ':'
    notFollowedBy $ chunk ":}"
    c <- some $ noneOf ['\n', '\0', ':']
    pure (T.pack r, T.pack c)

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
