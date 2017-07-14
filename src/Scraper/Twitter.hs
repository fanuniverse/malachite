module Scraper.Twitter (scrapeStatus) where

import Scraper
import Scraper.Internal

import Data.Monoid ((<>))

scrapeStatus :: (MonadHTTP m) => String -> m (Maybe Scraped)
scrapeStatus url = go <$> (snd <$> fetchPage url [])
  where
    go page = Just Scraped
        { imageUrl = sCdnImageUrl <> ":orig"
        , thumbnailUrl = sCdnImageUrl <> ":small"
        , artist = Just sArtist
        , pageUrl = Just sPageUrl }
      where
        sArtist = regexMatch "twitter.com/(.+?)/" sPageUrl
        sPageUrl = firstMetaContent "og:url" page
        sCdnImageUrl = regexMatch "(.+):large\\z"
          (firstMetaContent "og:image" page)
