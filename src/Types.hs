{-# LANGUAGE OverloadedStrings, QuasiQuotes, DeriveGeneric #-}
module Types (Product(..), User(..), Rating(..), Recyclability(..), Image(..)) where

import GHC.Generics

import Data.Text (Text)

import Data.Monoid ((<>))
import Data.Aeson
import Data.ByteString
import Data.Int

import Data.UUID

import Data.Time.Clock

import Web.Scotty

data Image = Image {
  imageId :: UUID,
  imageData :: ByteString,
  imageProductId :: Text
  } deriving (Eq, Show)

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

instance ToJSON Product where
  toJSON (Product productId name manufacturer) = object ["id" .= productId, "name" .= name, "manufacturer" .= manufacturer]
  toEncoding (Product productId name manufacturer) = pairs ("id" .= productId <> "name" .= name <> "manufacturer" .= manufacturer)

data Recyclability = NonRecyclable | Recyclable | Compostable deriving (Eq, Show, Generic)

instance ToJSON Recyclability where
  toEncoding = genericToEncoding defaultOptions
instance Parsable Recyclability where
  parseParam "non_recyclable" = Right NonRecyclable
  parseParam "recyclable" = Right Recyclable
  parseParam "compostable" = Right Compostable
  parseParam _ = Left "Unkown recyclability"

data Rating = Rating {
  ratingUserId :: UUID,
  ratingProductId :: Text,
  grade :: Int32,
  vendor :: Text,
  posted :: UTCTime,
  plastic :: Text,
  weight :: Int32,
  recyclable :: Recyclability
} deriving (Eq, Show, Generic)

instance ToJSON Rating where
  toEncoding = genericToEncoding defaultOptions
  {-toJSON (Rating uid pid grade vendor posted plastic weight recyc) =
    object ["uid" .= uid, "pid" .= pid ]
    -}
