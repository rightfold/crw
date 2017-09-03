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

module CRW.C.LexSpec
  ( spec
  ) where

import CRW.C.Lex
import Test.Hspec

import qualified Text.Parsec as P

spec :: Spec
spec =
  describe "lexeme" $ do
    it "skips comments" $
      P.parse (space *> lexeme) "" "  * foo\nAPPLICATION\n* bar  "
        `shouldBe` Right ApplicationKeyword

    it "lexes punctuation" $
      P.parse (space *> lexeme) "" "  ,  "
        `shouldBe` Right CommaPunc

    it "lexes keywords" $
      P.parse (space *> lexeme) "" "  APPLICATION  "
        `shouldBe` Right ApplicationKeyword

    it "lexes identifiers" $
      P.parse (space *> lexeme) "" "  foo  "
        `shouldBe` Right (Identifier "foo")

    it "lexes text literals" $
      P.parse (space *> lexeme) "" "  \"foo\"  "
        `shouldBe` Right (TextLiteral "foo")
