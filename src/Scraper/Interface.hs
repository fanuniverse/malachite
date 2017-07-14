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
    = DeviantArt.scrapePost url
  | url =~ ("\\Ahttps?://(www.)?fav\\.me/.+"                :: String)
    = DeviantArt.scrapePost =<< redirectedFrom url
  | url =~ ("\\Ahttps?://.+\\.deviantart\\.net/.+d.+"       :: String)
    = DeviantArt.scrapeCDN url
  | url =~ ("\\Ahttps?://.+\\.tumblr\\.com/(post|image)/.+" :: String)
    = Tumblr.scrapePost url
  -- The clause below detects a custom Tumblr domain. If it turns out
  -- there are other sites with a similar URL structure, they can be
  -- matched using Alternative (<|>), but the Tumblr scraper would need
  -- to be changed first to return Nothing for a non-200 API response.
  | url =~ ("\\Ahttps?://.+/(post|image)/.+"                :: String)
    = Tumblr.scrapePost url
  -- If a URL ends with an image extension, we may as well assume it
  -- points to an image and return it instead of Nothing.
  | url =~ ("\\Ahttps?://.*\\.(jpg|jpeg|png|gif|svg)"       :: String)
    = return $ Just Scraped
      { imageUrl = Text.pack url
      , thumbnailUrl = Text.pack url
      , artist = Nothing
      , pageUrl = Nothing }
  | otherwise
    = return Nothing
