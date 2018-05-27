{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module GetRatingSummary (getRatingSummary) where

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

getRatingSummary :: Connection -> S.ScottyM ()
getRatingSummary conn =
  S.get "/rating/:id/summary" $ do
    ean <- S.param "id"
    let rw1 = (,) <$> D.value D.int4 <*> D.value D.int4
        --sq = "select userid, grade, vendor, pl_type, pl_weight, recyclable from ratings where productid = $1"
        sq1 = "select avg(grade), avg(pl_weight) from ratings where productid = $1"
        ienc1 = contramap TE.encodeUtf8 (E.value E.unknown)
        st1 = statement sq1 ienc1 (D.singleRow rw1) True
    res <- lift $ flip run conn $ query (ean :: Text) st1
    case res of
      Left err -> S.raise . T.pack . show $ err
      Right val -> let val1 = val
  -- where
  --   makeJson (grd, wgt) (vdr, ptp, prc) = S.json [grd, vdr, ptp, wgt, prc]
