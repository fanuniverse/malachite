{-# LANGUAGE ScopedTypeVariables #-}

module Camo where

import Scraper

import Data.Hex (hex)
import Data.Text (Text)
import Data.Monoid ((<>))

import Crypto.MAC.HMAC
import Crypto.Hash.Algorithms

import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text as Text

camo :: Text -> Text -> Text -> Text
camo host key url = host <> "/" <> digest <> "/" <> encodedUrl
  where
    encodedUrl = (Text.toLower . Text.pack . hex . Text.unpack) url
    digest = (Text.pack . show . hmacGetDigest) urlHmac
    urlHmac :: HMAC SHA1 = hmac (encodeUtf8 key) (encodeUtf8 url)

camoifyScraped :: Text -> Text -> Scraped -> Scraped
camoifyScraped host key scraped = scraped
  { thumbnailUrl = camo host key (thumbnailUrl scraped) }
