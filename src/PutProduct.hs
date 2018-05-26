{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module PutProduct (putProduct) where

import qualified Web.Scotty as S

import Hasql.Session
import Hasql.Connection
import qualified Hasql.Decoders as D
import Hasql.Query

import qualified Data.Text.Lazy as T

import Control.Monad.Trans.Class

import Types
import Parser

putProduct :: Connection -> S.ScottyM ()
putProduct conn =
  S.put "/product/:id" $ do
    ean <- S.param "id"
    name <- S.param "name"
    manufacturer <- S.param "manufacturer"
    let sq = "insert into products (id, name, manufacturer) values ($1, $2, $3)"
        st = statement sq productP D.unit True
        pr = Product ean name (Just manufacturer)
    res <- lift $ flip run conn $ query pr st
    case res of
      Left err -> S.raise . T.pack . show $ err
      Right _ -> S.json pr
