module Scraper.Interface (scrape) where

import Text.Regex.PCRE ((=~))

import Scraper
import Scraper.Internal (MonadHTTP, redirectedFrom, httpUrl)
import qualified Scraper.Deviantart as DA
import qualified Scraper.Tumblr as T

scrape :: (MonadHTTP m) => String -> m (Maybe Scraped)
scrape url = case httpUrl url of
  Just validUrl -> scrapeUrl validUrl
  Nothing -> return Nothing

scrapeUrl :: (MonadHTTP m) => String -> m (Maybe Scraped)
scrapeUrl url
  | url =~ ("\\Ahttps?://.+\\.deviantart\\.com/.+"          :: String)
    = DA.fromPost url
  | url =~ ("\\Ahttps?://(www.)?fav\\.me/.+"                :: String)
    = redirectedFrom url >>= DA.fromPost
  | url =~ ("\\Ahttps?://.+\\.deviantart\\.net/.+d.+"       :: String)
    = DA.fromCDN url
  | url =~ ("\\Ahttps?://.+\\.tumblr\\.com/(post|image)/.+" :: String)
    = T.fromPost url
  | url =~ ("\\Ahttps?://.*\\.(jpg|jpeg|png|gif|svg)"       :: String)
    = return $ Just Scraped
      { imageUrl = url
      , thumbnailUrl = url
      , artist = Nothing
      , pageUrl = Nothing }
  | otherwise
    = return Nothing
