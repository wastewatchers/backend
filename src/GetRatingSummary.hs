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

import Data.Aeson

-- import Data.Time.Clock
-- import Data.UUID
import Control.Monad
import Data.Maybe
import Data.Text.Encoding as TE
import qualified Hasql.Encoders as E
import Data.Text (Text)
import Data.Functor.Contravariant

summaryR = (,,,,,) <$> D.value D.text <*> D.value D.text <*> D.value D.text <*> D.value D.text <*> D.value D.float8 <*> D.value D.float8

getSummaryS = statement sq (E.value E.text) (D.singleRow summaryR) True
    where
        sq = "select p.name, p.vendor, max(r.recyclable), max(r.pl_type), avg(r.grade), avg(r.pl_weight) from ratings r, products p where p.id = r.productid and r.productid = $1 group by p.id having r.pl_weight <> 0"

getImagesS = statement sq (E.value E.text) (D.rowsList (D.value D.uuid)) True
    where
        sq = "select i.id from products p, product_images i where p.id = i.productid and p.id = $1"

getRatingSummary :: Connection -> S.ScottyM ()
getRatingSummary conn = S.get "/rating/:id/summary" $ do
    ean <- S.param "id"
    obj <- runQuery $ do
        (nm, vd, re, pt, ag, aw) <- query ean getSummaryS
        imgs <- query ean getImagesS
        pure $ object [
            "name" .= nm, "vendor" .= vd, "recyclable" .= re,
            "images" .= imgs, "plastic_type" .= pt,
            "average_grade" .= ag, "average_weight" .= aw ]
    S.json obj
  where
    handleResult = either (S.raise . T.pack . show) pure
    runQuery = handleResult <=< lift . flip run conn

