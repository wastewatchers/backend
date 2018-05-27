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
import Data.Maybe
import Data.Text.Encoding as TE
import qualified Hasql.Encoders as E
import Data.Text (Text)
import Data.Functor.Contravariant

data Summary = Summary {
    name :: Text,
    recyclable :: Recyclability,
    plasticType :: Text,
    avgGrade :: Double,
    avgWeight :: Double
    } deriving (Eq, Show)

summaryR = (,,,,) <$> D.value D.text <*> D.value D.text <*> D.value D.text <*> D.value D.float8 <*> D.value D.float8

getSummaryS = statement sq ienc (D.singleRow summaryR) True
    where
        sq = "select p.name, max(r.recyclable), max(r.pl_type), avg(r.grade), avg(r.pl_weight) from ratings r, products p where p.id = r.productid and r.productid = $1 group by p.id"
        ienc = E.value E.text

getImagesS = statement sq ienc (D.rowsList (D.value D.uuid)) True
    where
        sq = "select i.id from products p, product_images i where p.id = i.productid and p.id = $1"
        ienc = E.value E.text

getRatingSummary :: Connection -> S.ScottyM ()
getRatingSummary conn =
  S.get "/rating/:id/summary" $ do
    ean <- S.param "id"
    res1 <- lift $ run (query (ean :: Text) getSummaryS) conn
    summ <- case res1 of
      Left err -> S.raise . T.pack . show $ err
      Right val -> pure val
    res2 <- lift $ run (query (ean :: Text) getImagesS) conn
    imgs <- case res2 of
      Left err -> S.raise . T.pack . show $ err
      Right val -> pure val
    let (nm, re, pt, ag, aw) = summ
    S.json $ object [
        "name" .= nm, "recyclable" .= re,
        "images" .= imgs, "plastic_type" .= pt,
        "average_grade" .= ag, "average_weight" .= aw ]
