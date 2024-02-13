module Parser (parse, Chunk(..), Replacements) where

import Data.Void (Void)
import Data.Maybe (fromJust)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char (space, hspace, hspace1, eol)
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

quoted :: Parser Text -> Parser String
quoted p = f <|> some
  ( do
    notFollowedBy $ void p <|> hspace1
    noneOf ['\n', '\0']
  )
  where
  f :: Parser String
  f = f' (single '"') <|> f' (single '\'')
    where
    f' :: Parser a -> Parser String
    f' p = p >> manyTill charLiteral p

replace :: Parser Chunk
replace = do
  chunk "{!"
  r <- paramName
  chunk "!}"
  pure $ Replace r

paramName :: Parser Text
paramName = T.pack <$> some (satisfy c)
  where
  c :: Char -> Bool
  c x = not (isSpace x) && x `notElem` ['!', ':']

parameter :: Parser (Text, Text)
parameter = do
  r <- try (space >> paramName)
  hspace
  single ':'
  hspace
  c <- quoted $ chunk ":}"
  pure (r, T.pack c)

insert :: Parser Chunk
insert = do
  hspace
  chunk "{:"
  space
  r <- quoted $ chunk ":}"
  a <- M.fromList <$> many parameter
  space
  chunk ":}"
  hspace
  pure $ Insert r a

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
