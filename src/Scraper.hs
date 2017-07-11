{-# LANGUAGE DeriveGeneric #-}

module Scraper where

import GHC.Generics
import Data.Aeson

data Scraped = Scraped
  { imageUrl     :: String
  , thumbnailUrl :: String
  , artist       :: Maybe String
  , pageUrl      :: Maybe String }
  deriving (Generic, Show, Eq)

instance ToJSON Scraped where
  toEncoding = genericToEncoding defaultOptions
