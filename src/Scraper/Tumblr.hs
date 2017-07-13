module Scraper.Tumblr (scrapePost) where

import Scraper
import Scraper.Internal

import Text.Regex.PCRE ((=~))

import Text.HTML.TagSoup (isTagOpenName)

scrapePost :: (MonadHTTP m) => String -> m (Maybe Scraped)
scrapePost url = go =<< (snd <$> fetchPage apiUrl [])
  where
    apiUrl = blog ++ "/api/read?id=" ++ postId
    [[_, blog, postId]] = url =~ postRegex
    go doc = return $ Just Scraped
      { imageUrl = sImageUrl
      , thumbnailUrl = sThumbnailUrl
      , artist = Just sArtist
      , pageUrl = Just sPageUrl }
      where
        sImageUrl = firstText
          (hasAttr "max-width" "1280" <@ doc)
        sThumbnailUrl = firstText
          (hasAttr "max-width" "500" <@ doc)
        sArtist = firstAttr "name"
          (isTagOpenName "tumblelog" <@ doc)
        sPageUrl = firstAttr "url-with-slug"
          (isTagOpenName "post" <@ doc)

postRegex :: String
postRegex = "\\A(https?://.+\\.tumblr\\.com)/(?:post|image)/(\\d+)"
