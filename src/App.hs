{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module App (runApp) where

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

import Data.Time.Clock
import Data.Maybe

import Control.Monad.Trans.Class

import Types
import Parser

cfg :: Settings
cfg = settings "localhost" 5432 "tobias" "" "wastewatchers"

app' :: Connection -> S.ScottyM ()
app' conn = do
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

  S.get "/product/:id" $ do
    ean <- S.param "id"
    let rw = (,,) <$> D.value D.text <*> D.value D.text <*> D.value D.text
        sq = "select id, name, manufacturer from products where id = $1"
        ienc = contramap TE.encodeUtf8 (E.value E.unknown)
        st = statement sq ienc (D.singleRow rw) True
    res <- lift $ flip run conn $ query (ean :: Text) st
    case res of
      Left err -> S.raise . T.pack . show $ err
      Right val -> S.json val

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
      Right _ -> S.text "test"

  S.get "/" $ do
    S.text "hello"

  S.get "/some-json" $ do
    S.json $ object ["foo" .= Number 23, "bar" .= Number 42]

runApp :: IO ()
runApp = do
  ac <- acquire cfg
  case ac of
    Left err -> error $ show err
    Right c -> S.scotty 8080 (app' c)
