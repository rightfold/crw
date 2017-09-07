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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CRW.C.Check
  ( Error

  , Check
  , CheckT
  , runCheck
  , runCheckT

  , checkProgram
  , checkStatement
  , checkExpression
  ) where

import CRW.C.Syntax (AuthorizationPhase (..), DisplayPhase (..), Expression (..), Program (..), Statement (..), Type (..))
import Control.Category ((>>>))
import Control.Comonad.Cofree (Cofree (..))
import Control.Monad ((>=>))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Functor.Foldable (Mu, project)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Void (absurd)



data Error



type Check = CheckT Identity

newtype CheckT m a = CheckT (ExceptT Error m a)
  deriving (Functor, Applicative, Monad)

runCheck :: Check a -> Either Error a
runCheck = runIdentity . runCheckT

runCheckT :: CheckT m a -> m (Either Error a)
runCheckT (CheckT a) = runExceptT a



checkProgram :: Program (Mu Expression) -> Check (Program (Cofree Expression Type))
checkProgram (View level name (AuthorizationPhase authorizationPhase) (DisplayPhase displayPhase)) =
  View level name
  <$> (AuthorizationPhase <$> traverse checkStatement authorizationPhase)
  <*> (DisplayPhase <$> traverse checkStatement displayPhase)
checkProgram (Action void) = absurd void

checkStatement :: Statement (Mu Expression) -> Check (Statement (Cofree Expression Type))
checkStatement (DisplayStatement arguments) =
  DisplayStatement <$> traverse checkExpression arguments
checkStatement FlushDisplayStatement = pure FlushDisplayStatement

checkExpression :: Mu Expression -> Check (Cofree Expression Type)
checkExpression = project >>> traverse checkExpression >=> \case
  e@(TextLiteralExpression _) -> pure $ TextType :< e
