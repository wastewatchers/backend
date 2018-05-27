{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module GetRatingSummary (getRatingSummary) where

import qualified Web.Scotty as S
import Hasql.Session
import Hasql.Connection
import qualified Hasql.Decoders as D
import Hasql.Query
import qualified Data.Text.Lazy as T
import Control.Monad.Trans.Class

import Data.Aeson

-- import Data.Time.Clock
import Data.UUID
import Control.Monad
import qualified Hasql.Encoders as E
import Data.Text (Text)

summaryR :: D.Row (Text, Text, Text, Double, Double)
summaryR = (,,,,) <$> D.value D.text <*> D.value D.text <*> D.value D.text <*> D.value D.float8 <*> D.value D.float8

getSummaryS :: Query Text (Text, Text, Text, Double, Double)
getSummaryS = statement sq (E.value E.text) (D.singleRow summaryR) True
    where
        sq = "select p.name, max(r.recyclable), max(r.pl_type), avg(r.grade), avg(r.pl_weight) from ratings r, products p where p.id = r.productid and r.pl_weight > 0 and r.productid = $1 group by p.id"

getImagesS :: Query Text [UUID]
getImagesS = statement sq (E.value E.text) (D.rowsList (D.value D.uuid)) True
    where
        sq = "select i.id from products p, product_images i where p.id = i.productid and p.id = $1"

getRatingSummary :: Connection -> S.ScottyM ()
getRatingSummary conn = S.get "/rating/:id/summary" $ do
    ean <- S.param "id"
    runQuery $ do
        (nm, re, pt, ag, aw) <- query ean getSummaryS
        imgs <- query ean getImagesS
        pure $ object [
            "name" .= nm, "recyclable" .= re,
            "images" .= imgs, "plastic_type" .= pt,
            "average_grade" .= ag, "average_weight" .= aw ]
  where
    handleResult = either (S.raise . T.pack . show) pure
    runQuery = S.json <=< handleResult <=< lift . flip run conn

