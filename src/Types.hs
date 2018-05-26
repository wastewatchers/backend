{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Types (Product(..), User(..)) where

import Data.Text (Text)

import Data.UUID

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
