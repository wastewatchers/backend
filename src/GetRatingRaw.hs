{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module GetRatingRaw (getRatingRaw) where

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

getRatingRaw :: Connection -> S.ScottyM ()
getRatingRaw conn =
  S.get "/rating/:id/raw" $ do
    ean <- S.param "id"
    let rw = (,,,,,) <$> D.value D.uuid <*> D.value D.int4 <*> D.value D.text <*> D.value D.text <*> D.value D.int4 <*> D.value D.text
        sq = "select userid, grade, vendor, pl_type, pl_weight, recyclable from ratings where productid = $1"
        ienc = contramap TE.encodeUtf8 (E.value E.unknown)
        st = statement sq ienc (D.singleRow rw) True
    res <- lift $ flip run conn $ query (ean :: Text) st
    case res of
      Left err -> S.raise . T.pack . show $ err
      Right val -> S.json val
