{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Example (runApp) where

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

data User = User {
  userId :: UUID,
  username :: Text
  } deriving (Eq, Show)

data Product = Product {
  productId :: Text,
--  asin :: Maybe Text,
  name :: Text,
  manufacturer :: Maybe Text
  } deriving (Eq, Show)
-- addProduct(id: EAN13,name: text, manufacturer: text)
prod :: E.Params Product
prod =
     contramap (TE.encodeUtf8 . productId) (E.value E.unknown)
  <> contramap name (E.value E.text)
  <> contramap manufacturer (E.nullableValue E.text)

  --  <> contramap asin (nullableParam text)
cfg :: Settings
cfg = settings "localhost" 5432 "tobias" "" "wastewatchers"

app' :: Connection -> S.ScottyM ()
app' conn = do
  S.put "/product/:id" $ do
    ean <- S.param "id"
    name <- S.param "name"
    manufacturer <- S.param "manufacturer"
    let sq = "insert into products (id, name, manufacturer) values ($1, $2, $3)"
        st = statement sq prod D.unit True
    res <- lift $ flip run conn $ query (Product ean name (Just manufacturer)) st
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
