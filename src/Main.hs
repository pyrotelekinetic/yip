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

import System.Environment (getArgs)
import Data.Text as T

main = do
  (a : as) <- getArgs
  case a of
    [] -> showHelp
    "--help" -> showHelp

showHelp :: IO ()
showHelp = T.putStrLn $ intercalate "\n" helpTxt
  where
  helpTxt =
    [ "usage:"
    , "\tyip <file>"
    , "\tPreproccess given file"
    ]
