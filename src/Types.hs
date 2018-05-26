{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Types (Product(..), User(..)) where

import Data.Text (Text)

import Data.Monoid ((<>))
import Data.Aeson
import Data.ByteString

import Data.UUID

data Picture = Picture {
  pictureId :: Int,
  imageData :: ByteString,
  picProductId :: Text
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

