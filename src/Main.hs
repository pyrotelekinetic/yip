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
import System.FilePath (dropFileName)
import System.Directory (withCurrentDirectory)

import Parser
import Command

main = do
  o <- parseOpts
  c <- T.readFile $ file o
  let i = parse c
  c' <- withRelativeDir (file o) $ process i
  T.putStr c'

showHelp :: IO ()
showHelp = T.putStrLn $ T.unlines helpTxt
  where
  helpTxt =
    [ "usage:"
    , "\tyip <file>"
    , "\tPreproccess given file"
    ]

withRelativeDir :: FilePath -> IO a -> IO a
withRelativeDir = withCurrentDirectory . dropFileName

process :: [Line] -> IO Text
process [] = pure ""
process [Literal l] = pure l
process (Literal l : xs) = ((l <> "\n") <>) <$> process xs
process (Insert f : xs) = do
  c <- T.readFile f
  (c <>) <$> process xs
