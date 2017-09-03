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

module CRW.C.Lex
  ( -- * Parser type
    Parser
  , Lexeme (..)

    -- * Lexemes
  , lexeme

    -- * Ambient
  , space
  ) where

import Prelude hiding (head, tail)

import Data.Functor (void)
import Data.Text (Text)

import qualified Data.Text as Text
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC



type Parser = P.Parsec Text ()

data Lexeme
  = CommaPunc

  | ApplicationKeyword
  | DisplayKeyword
  | FlushKeyword
  | SystemKeyword
  | UserKeyword
  | ViewKeyword

  | Identifier Text

  | TextLiteral Text
  deriving (Eq, Show, Read)



lexeme :: Parser Lexeme
lexeme = commaPunc P.<|> keywordOrIdentifier P.<|> textLiteral

commaPunc :: Parser Lexeme
commaPunc = CommaPunc <$ spaced (PC.char ',')

keywordOrIdentifier :: Parser Lexeme
keywordOrIdentifier = spaced $ do
  name <- (Text.pack .) . (:) <$> head <*> tail
  pure $ case name of
    "APPLICATION" -> ApplicationKeyword
    "DISPLAY" -> DisplayKeyword
    "FLUSH" -> FlushKeyword
    "SYSTEM" -> SystemKeyword
    "USER" -> UserKeyword
    "VIEW" -> ViewKeyword
    _ -> Identifier name
  where head = PC.oneOf $ ['A'..'Z'] ++ ['a'..'z']
        tail = P.many . PC.oneOf $ ['A'..'Z'] ++ ['a'..'z']

textLiteral :: Parser Lexeme
textLiteral = spaced $ do
  _ <- PC.char '"'
  value <- Text.pack <$> P.many (PC.noneOf ['"'])
  _ <- PC.char '"'
  pure $ TextLiteral value



space :: Parser ()
space = void $ P.many (void PC.space P.<|> void comment)
  where comment = PC.char '*' *> P.many (PC.noneOf ['\n'])

spaced :: Parser a -> Parser a
spaced = (<* space)
