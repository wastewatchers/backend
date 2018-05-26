{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Parser (productP, ratingP) where

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

userP :: E.Params User
userP =
     contramap userId (E.value E.uuid)
  <> contramap username (E.value E.text)

ratingP :: E.Params Rating
ratingP =
     contramap ratingUserId (E.value E.uuid)
  <> contramap ratingProductId (E.value E.text)
  <> contramap grade (E.value E.int4)
  <> contramap vendor (E.value E.text)
  <> contramap posted (E.value E.timestamptz)
  <> contramap plastic (E.value E.text)
  <> contramap weight (E.value E.int4)
  <> contramap recyclable (E.value $ E.enum rtt)
  where
    rtt NonRecyclable = "non_recyclable"
    rtt Recyclable = "recyclable"
    rtt Compostable = "compostable"
