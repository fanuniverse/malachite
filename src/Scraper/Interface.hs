module Scraper.Interface (scrape) where

import Text.Regex.PCRE ((=~))
import qualified Data.Text as Text

import Scraper
import Scraper.Internal (MonadHTTP, redirectedFrom, httpUrl)
import qualified Scraper.Deviantart as DeviantArt
import qualified Scraper.Tumblr as Tumblr

scrape :: (MonadHTTP m) => String -> m (Maybe Scraped)
scrape url = case httpUrl url of
  Just validUrl -> scrapeUrl validUrl
  Nothing -> return Nothing

scrapeUrl :: (MonadHTTP m) => String -> m (Maybe Scraped)
scrapeUrl url
  | url =~ ("\\Ahttps?://.+\\.deviantart\\.com/.+"          :: String)
    = DeviantArt.fromPost url
  | url =~ ("\\Ahttps?://(www.)?fav\\.me/.+"                :: String)
    = DeviantArt.fromPost =<< redirectedFrom url
  | url =~ ("\\Ahttps?://.+\\.deviantart\\.net/.+d.+"       :: String)
    = DeviantArt.fromCDN url
  | url =~ ("\\Ahttps?://.+\\.tumblr\\.com/(post|image)/.+" :: String)
    = Tumblr.fromPost url
  | url =~ ("\\Ahttps?://.*\\.(jpg|jpeg|png|gif|svg)"       :: String)
    = return $ Just Scraped
      { imageUrl = Text.pack url
      , thumbnailUrl = Text.pack url
      , artist = Nothing
      , pageUrl = Nothing }
  | otherwise
    = return Nothing
