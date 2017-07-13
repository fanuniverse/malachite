{-# LANGUAGE DeriveGeneric #-}

module Scraper where

import GHC.Generics
import Data.Aeson

import Data.Text (Text)

data Scraped = Scraped
  { imageUrl     :: Text
  , thumbnailUrl :: Text
  , artist       :: Maybe Text
  , pageUrl      :: Maybe Text }
  deriving (Generic, Show, Eq)

instance ToJSON Scraped where
  toEncoding = genericToEncoding defaultOptions
