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

{-# LANGUAGE TemplateHaskell #-}

module CRW.C.Syntax
  ( -- * Programs
    Program (..)
  , Level (..)

    -- * Views
  , ViewBody (..)

    -- * Statements
  , Statement (..)

    -- * Expressions
  , Expression (..)
  ) where

import Data.Eq.Deriving (deriveEq1)
import Data.Text (Text)
import Data.Void (Void)
import Text.Read.Deriving (deriveRead1)
import Text.Show.Deriving (deriveShow1)



data Program e
  = View Level Text (ViewBody e)
  | Action Void
  deriving (Eq, Functor, Show, Read)

data Level = SystemLevel | ApplicationLevel | UserLevel
  deriving (Eq, Show, Read)



newtype ViewBody e =
  ViewBody [Statement e]
  deriving (Eq, Functor, Show, Read)



data Statement e
  = DisplayStatement [e]
  | FlushStatement
  deriving (Eq, Functor, Show, Read)



data Expression a
  = TextLiteralExpression Text
  deriving (Eq, Functor, Show, Read)
$(deriveEq1 ''Expression)
$(deriveShow1 ''Expression)
$(deriveRead1 ''Expression)
