{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Parser (productP) where

import           Data.Aeson (Value(..), object, (.=))
import           Network.Wai (Application)
import qualified Web.Scotty as S

--import Database.PostgreSQL.Simple
import Hasql.Connection
import Hasql.Session
import qualified Hasql.Encoders as E
import qualified Hasql.Decoders as D
--import Hasql.Statement
import Hasql.Query

import Data.Text (Text)
import Data.Text.Encoding as TE
import qualified Data.Text.Lazy as T

import Data.UUID
import Data.Functor.Contravariant
import Data.Monoid ((<>))

import Control.Monad.Trans.Class

import Types

productP :: E.Params Product
productP =
     contramap productId (E.value E.text)
  <> contramap name (E.value E.text)
  <> contramap manufacturer (E.nullableValue E.text)

