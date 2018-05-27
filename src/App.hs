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

import Network.Wai.Middleware.RequestLogger

import Data.Text (Text)
import Data.Text.Encoding as TE
import qualified Data.Text.Lazy as T
import qualified Data.ByteString.Lazy as BS

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
import GetProduct
import GetRatingCount
import GetRatingRaw
import PutImage

cfg :: Settings
cfg = settings "localhost" 5432 "tobias" "" "wastewatchers"

app' :: Connection -> S.ScottyM ()
app' conn = do
  S.middleware logStdout

  putProduct conn
  putRating conn
  putImage conn
  getProduct conn
  getRatingCount conn
  getRatingRaw conn

  S.get "/image/:imageid" $ do
    uuid <- S.param "imageid"
    let rw = D.value D.bytea
        sq = "select image_data from product_images where id = $1"
        ienc = E.value E.text
        st = statement sq ienc (D.singleRow rw) True
    res <- lift $ flip run conn $ query uuid st
    case res of
      Left err -> S.raise . T.pack . show $ err
      Right val -> do
        S.setHeader "Content-Type" "image/jpeg"
        S.raw $ BS.fromStrict val

runApp :: IO ()
runApp = do
  ac <- acquire cfg
  case ac of
    Left err -> error $ show err
    Right c -> S.scotty 8080 (app' c)
