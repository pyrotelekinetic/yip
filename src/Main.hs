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

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Set (Set)
import qualified Data.Set as S
import System.FilePath (dropFileName)
import System.Directory (withCurrentDirectory)
import Control.Applicative (liftA2)

import Parser
import Command

main = do
  o <- parseOpts
  r <- process (noRecurse o) $ input o
  case r of
    (Left e) -> putStrLn e
    (Right t) ->
      case output o of
        "-" -> T.putStr t
        f -> T.writeFile f t

withRelativeDir :: FilePath -> IO a -> IO a
withRelativeDir = withCurrentDirectory . dropFileName

process :: Bool -> FilePath -> IO (Either String Text)
process True = (pure <$>) . processNoRecurse
process False = processRecurse S.empty

-- | Process with recursion
processRecurse :: Set FilePath -> FilePath -> IO (Either String Text)
processRecurse s x = withRelativeDir x . f s . parse =<< T.readFile x
  where
  f :: Set FilePath -> [Line] -> IO (Either String Text)
  f _ [] = pure $ Right ""
  f _ [Literal l] = pure $ Right l
  f s (Literal x : xs) = (Right (x <> "\n") <<>>) <$> f s xs
  f s (Insert x : xs)
    | S.member x s = pure $ Left "Error: circular graph detected"
    | otherwise = do
      c <- processRecurse (S.insert x s) x
      (c <<>>) <$> f s xs

  (<<>>) :: Either e Text -> Either e Text -> Either e Text
  (<<>>) = liftA2 (<>)

-- | Process without recursion
processNoRecurse :: FilePath -> IO Text
processNoRecurse x = withRelativeDir x . f . parse =<< T.readFile x
  where
  f :: [Line] -> IO Text
  f [] = pure ""
  f [Literal l] = pure l
  f (Literal x : xs) = ((x <> "\n") <>) <$> f xs
  f (Insert x : xs) = do
    c <- T.readFile x
    (c <>) <$> f xs
