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

import PutProduct
import PutRating

cfg :: Settings
cfg = settings "172.16.56.232" 5432 "tobias" "" "wastewatchers"

app' :: Connection -> S.ScottyM ()
app' conn = do
  putProduct conn
  putRating conn

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

runApp :: IO ()
runApp = do
  ac <- acquire cfg
  case ac of
    Left err -> error $ show err
    Right c -> S.scotty 8080 (app' c)
