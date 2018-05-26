{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module GetProduct (getProduct) where

import qualified Web.Scotty as S
import Hasql.Session
import Hasql.Connection
import qualified Hasql.Decoders as D
import Hasql.Query
import qualified Data.Text.Lazy as T
import Control.Monad.Trans.Class
import Types
import Parser

-- import Data.Time.Clock
-- import Data.UUID
import Data.Maybe
import Data.Text.Encoding as TE
import qualified Hasql.Encoders as E
import Data.Text (Text)
import Data.Functor.Contravariant

getProduct :: Connection -> S.ScottyM ()
getProduct conn =
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
