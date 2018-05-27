{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module PutRating (putRating) where

import qualified Web.Scotty as S
import Hasql.Session
import Hasql.Connection
import qualified Hasql.Decoders as D
import Hasql.Query
import qualified Data.Text.Lazy as T
import Control.Monad.Trans.Class
import Types
import Parser

import Data.Time.Clock
import Data.UUID
import Data.Maybe

putRating :: Connection -> S.ScottyM ()
putRating conn =
  S.put "/rating/:id" $ do
    ean <- S.param "id"
    uid <- S.param "uid"
    grade <- S.param "grade"
    vendor <- S.param "vendor"
    ptype <- S.param "ptype" -- plastic type
    weight <- S.param "weight"
    time <- lift $ getCurrentTime
    recyclable <- S.param "recyclable"
    let sq = "insert into ratings \
              \(userid, productid, grade, vendor, posted, pl_type, pl_weight, recyclable) \
              \values ($1, $2, $3, $4, $5, $6, $7, $8)"
        st = statement sq ratingP D.unit True
        rt = Rating (fromJust . fromText $ uid) ean grade vendor time ptype weight (recyclable :: Recyclability)
    res <- lift $ flip run conn $ query rt st
    case res of
      Left err -> S.raise . T.pack . show $ err
      Right _ -> S.text "test" --S.json rt
