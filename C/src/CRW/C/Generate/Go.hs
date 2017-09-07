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

module CRW.C.Generate.Go
  ( codegenProgram
  , codegenStatement
  , codegenExpression
  ) where

import CRW.C.Syntax (AuthorizationPhase (..), DisplayPhase (..), Expression (..), Program (..), Statement (..), Level (..), Type)
import Control.Comonad.Cofree (Cofree (..))
import Data.ByteString.Builder (Builder)
import Data.ByteString.Short (ShortByteString)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8Builder)
import Data.Void (absurd)

import qualified Data.ByteString.Builder as Builder



codegenProgram :: Program (Cofree Expression Type) -> Builder
codegenProgram (View level name authorizationPhase displayPhase) = mempty
  <> b "// Level: " <> codegenLevel <> b "\n"
  <> b "// Name: " <> codegenName <> b "\n"
  <> b "package CRW" <> codegenLevel <> codegenName <> b "\n"
  <> b "import (\n"
  <> b "CRWRTDisplay \"CRW/RT/Display\"\n"
  <> b "CRWRTProgram \"CRW/RT/Program\"\n"
  <> b ")\n"
  <> b "var Program CRWRTProgram.Program = &CRWRTProgram.View{\n"
  <> b "AuthorizationPhase: AuthorizationPhase,\n"
  <> b "DisplayPhase: DisplayPhase,\n"
  <> b "}\n"
  <> codegenAuthorizationPhase authorizationPhase <> b "\n"
  <> codegenDisplayPhase displayPhase
  where
    codegenLevel = case level of
      SystemLevel -> b "System"
      ApplicationLevel -> b "Application"
      UserLevel -> b "User"
    codegenName = s name
codegenProgram (Action void) = absurd void

codegenAuthorizationPhase :: AuthorizationPhase (Cofree Expression Type) -> Builder
codegenAuthorizationPhase (AuthorizationPhase body) = mempty
  <> b "func AuthorizationPhase(display CRWRTDisplay.Display) error {\n"
  <> foldMap (codegenStatement <> const (b "\n")) body
  <> b "return nil\n"
  <> b "}"

codegenDisplayPhase :: DisplayPhase (Cofree Expression Type) -> Builder
codegenDisplayPhase (DisplayPhase body) = mempty
  <> b "func DisplayPhase(display CRWRTDisplay.Display) error {\n"
  <> foldMap (codegenStatement <> const (b "\n")) body
  <> b "return nil\n"
  <> b "}"



codegenStatement :: Statement (Cofree Expression Type) -> Builder
codegenStatement (DisplayStatement arguments) = mempty
  <> b "{\n"
  <> foldMap (uncurry codegenExpression <> const (b "\n")) arguments'
  <> b "err := display.Display(" <> foldMap (fst <> const (b ", ")) arguments' <> b ")\n"
  <> b "if err != nil {\n"
  <> b "return err\n"
  <> b "}\n"
  <> b "}"
  where arguments' = [ (Builder.string7 ("temp" <> show i), argument)
                     | i <- [0..] :: [Int]
                     | argument <- arguments ]
codegenStatement FlushDisplayStatement = mempty
  <> b "display.Flush()"



codegenExpression :: Builder -> Cofree Expression Type -> Builder
codegenExpression rname (_ :< TextLiteralExpression value) = mempty
  <> rname <> " := " <> b "\"" <> s value <> b "\""



b :: ShortByteString -> Builder
b = Builder.shortByteString

s :: Text -> Builder
s = encodeUtf8Builder
