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

module CRW.C.ParseSpec
  ( spec
  ) where

import CRW.C.Parse
import Test.Hspec

import CRW.C.Lex (lexeme, space)
import CRW.C.Syntax (Expression (..), Level (..), Program (..), Statement (..), ViewBody (..))
import Data.Functor.Foldable (embed)

import qualified Data.Text.IO as Text.IO
import qualified Text.Parsec as P

spec :: Spec
spec =
  describe "program" $
    it "parses VIEW SYSTEM Version" $ do
      source <- Text.IO.readFile "../Core/src/Version.crw"
      let
        result =
          pure source
          >>= P.parse (space *> P.many lexeme <* P.eof) ""
          >>= P.parse (program <* P.eof) ""
        expected = Right $
          View SystemLevel
               "Version"
               (ViewBody [ DisplayStatement [ embed . TextLiteralExpression $ "Version"
                                            , embed . TextLiteralExpression $ "0.0.0"
                                            ]
                         , FlushStatement
                         ])
      result `shouldBe` expected
