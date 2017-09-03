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

module CRW.C.Parse
  ( -- * Parser type
    Parser

    -- * Programs
  , program

    -- * Views
  , view
  , viewBody

    -- * Statements
  , statement
  , displayStatement
  , flushStatement

    -- * Expressions
  , expression
  , textLiteralExpression
  ) where

import CRW.C.Lex (Lexeme (..))
import CRW.C.Syntax (Expression (..), Level (..), Program (..), Statement (..), ViewBody (..))
import Data.Functor.Foldable (Mu, embed)
import Data.Text (Text)

import qualified Text.Parsec as P



type Parser = P.Parsec [Lexeme] ()

program :: Parser (Program (Mu Expression))
program = view



view :: Parser (Program (Mu Expression))
view = do
  _ <- lexeme ViewKeyword
  level <- P.choice [ SystemLevel <$ lexeme SystemKeyword
                    , ApplicationLevel <$ lexeme ApplicationKeyword
                    , UserLevel <$ lexeme UserKeyword
                    ]
  name <- identifier
  body <- viewBody
  pure $ View level name body

viewBody :: Parser (ViewBody (Mu Expression))
viewBody = ViewBody <$> P.many statement



statement :: Parser (Statement (Mu Expression))
statement = P.choice [ displayStatement
                     , flushStatement
                     ]

displayStatement :: Parser (Statement (Mu Expression))
displayStatement = do
  _ <- lexeme DisplayKeyword
  arguments <- expression `P.sepBy` lexeme CommaPunc
  pure $ DisplayStatement arguments

flushStatement :: Parser (Statement (Mu Expression))
flushStatement = FlushStatement <$ lexeme FlushKeyword



expression :: Parser (Mu Expression)
expression = textLiteralExpression

textLiteralExpression :: Parser (Mu Expression)
textLiteralExpression = embed . TextLiteralExpression <$> textLiteral



lexeme :: Lexeme -> Parser Lexeme
lexeme l = P.try . (P.<?> show l) $ do
  l' <- P.anyToken
  if l == l'
    then pure l'
    else P.unexpected $ show l'

identifier :: Parser Text
identifier = P.try . (P.<?> "identifier") $ do
  l <- P.anyToken
  case l of
    Identifier v -> pure v
    _ -> P.unexpected $ show l

textLiteral :: Parser Text
textLiteral = P.try . (P.<?> "text literal") $ do
  l <- P.anyToken
  case l of
    TextLiteral v -> pure v
    _ -> P.unexpected $ show l
