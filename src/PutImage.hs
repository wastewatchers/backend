{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module PutImage where

import qualified Web.Scotty as S

import Hasql.Session
import Hasql.Connection
import qualified Hasql.Decoders as D
import Hasql.Query


import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T
import Data.UUID
import Data.Maybe

import Control.Monad.Trans.Class

import System.Random

import Types
import Parser

putImage :: Connection -> S.ScottyM ()
putImage conn =
  S.put "/product/:productid/image" $ do
    uuid <- lift $ randomIO
    productid <- S.param "productid"
    imagedata <- BS.toStrict <$> S.body
    let sq = "insert into product_images (id, productid, image_data) values ($1, $2, $3)"
        st = statement sq imageP D.unit True
        im = Image uuid imagedata productid
    res <- lift $ run (query im st) conn
    case res of
      Left err -> S.raise . T.pack . show $ err
      Right _ -> S.text ""
