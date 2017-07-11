module Camo where

import Scraper
import Strings

import Data.Hex (hex)
import Data.Char (toLower)

import Crypto.MAC.HMAC
import Crypto.Hash.Algorithms

camo :: String -> String -> String -> String
camo host key url =
  let digest = show $ hmacGetDigest ((hmac (bString key) (bString url)) :: HMAC SHA1)
      encodedUrl = map toLower (hex url)
  in host ++ "/" ++ digest ++ "/" ++ encodedUrl

camoifyScraped :: String -> String -> Scraped -> Scraped
camoifyScraped host key scraped = scraped
  { thumbnailUrl = camo host key (thumbnailUrl scraped) }
