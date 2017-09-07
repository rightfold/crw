-- Copyright 2017, rightfold
--
-- This file is part of CRW.
--
-- CRW is free software: you can redistribute it and/or modify it under the
-- terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- CRW is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
-- A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with CRW. If not, see <http://www.gnu.org/licenses/>.

module Main
  ( main
  ) where

import CRW.C.Check (checkProgram, runCheck)
import CRW.C.Generate.Go (codegenProgram)
import System.Environment (getArgs)
import System.IO (stdout)

import qualified CRW.C.Lex as Lex
import qualified CRW.C.Parse as Parse
import qualified Data.ByteString.Builder as Builder
import qualified Data.Text.IO as Text.IO
import qualified Text.Parsec as P

main :: IO ()
main = do
  [filename] <- getArgs
  source <- Text.IO.readFile filename
  let Right lexemes = P.parse (Lex.space *> P.many Lex.lexeme <* P.eof) "" source
      Right program = P.parse (Parse.program <* P.eof) "" lexemes
      Right program' = runCheck $ checkProgram program
      goProgram = codegenProgram program'
  Builder.hPutBuilder stdout goProgram
