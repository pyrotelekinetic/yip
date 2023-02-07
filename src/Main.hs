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

module Main where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Set (Set)
import qualified Data.Set as S
import System.FilePath (dropFileName)
import System.Directory (withCurrentDirectory)
import System.IO (hPutStrLn, stderr)
import Control.Applicative (liftA2)

import Parser
import Command

type SeenFiles = Set FilePath

main = do
  o <- parseOpts
  r <- process (noRecurse o) $ input o
  case r of
    (Left e) -> hPutStrLn stderr e
    (Right t) ->
      case output o of
        "-" -> T.putStr t
        f -> T.writeFile f t

withRelativeDir :: FilePath -> IO a -> IO a
withRelativeDir = withCurrentDirectory . dropFileName

process :: Bool -> FilePath -> IO (Either String Text)
process True = processNoRecurse M.empty
process False = processRecurse S.empty M.empty

(<<>>) :: Either e Text -> Either e Text -> Either e Text
(<<>>) = liftA2 (<>)

-- | Process with recursion
processRecurse :: SeenFiles -> Replacements -> FilePath -> IO (Either String Text)
processRecurse s m x = withRelativeDir x . f s m . parse =<< T.readFile x
  where
  f :: SeenFiles -> Replacements -> [Chunk] -> IO (Either String Text)
  f _ _ [] = pure $ Right ""
  f _ _ [Literal l] = pure $ Right l
  f s m (Newline : xs) = (Right "\n" <<>>) <$> f s m xs
  f s m (Literal x : xs) = (Right x <<>>) <$> f s m xs
  f s m (Replace r : xs) = case M.lookup r m of
    Nothing -> pure . Left $ "Error, no match found for " <> show r
    Just r' -> (Right r' <<>>) <$> f s m xs
  f s _ (Insert x m : xs)
    | S.member x s = pure $ Left "Error: circular graph detected"
    | otherwise = do
      c <- processRecurse (S.insert x s) m x
      (c <<>>) <$> f s m xs

-- | Process without recursion
processNoRecurse :: Replacements -> FilePath -> IO (Either String Text)
processNoRecurse r x = withRelativeDir x . f r . parse =<< T.readFile x
  where
  f :: Replacements -> [Chunk] -> IO (Either String Text)
  f _ [] = pure $ Right ""
  f _ [Literal l] = pure $ Right l
  f m (Newline : xs) = (Right "\n" <<>>) <$> f m xs
  f m (Literal x : xs) = (Right x <<>>) <$> f m xs
  f m (Replace r : xs) = case M.lookup r m of
    Nothing -> pure . Left $ "Error, no match found for " <> show r
    Just r' -> (Right r' <<>>) <$> f m xs
  f _ (Insert x m : xs) = do
    c <- T.readFile x
    (Right c <<>>) <$> f m xs
