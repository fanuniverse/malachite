module Scraper.Tumblr (fromPost) where

import Scraper
import Scraper.Internal

import Strings (toString)

import Text.Regex.PCRE ((=~))

import Text.HTML.TagSoup.Fast (parseTags)
import Text.HTML.TagSoup (isTagOpenName)

fromPost :: (MonadHTTP m) => String -> m (Maybe Scraped)
fromPost url = go =<< (parseTags . snd <$> fetchPage apiUrl [])
  where
    apiUrl = blog ++ "/api/read?id=" ++ postId
    [[_, blog, postId]] = url =~ postRegex
    go doc = return $ Just Scraped
      { imageUrl = sImageUrl
      , thumbnailUrl = sThumbnailUrl
      , artist = Just sArtist
      , pageUrl = Just sPageUrl }
      where
        sImageUrl = toString $ firstText
          (hasAttr "max-width" "1280" <@ doc)
        sThumbnailUrl = toString $ firstText
          (hasAttr "max-width" "500" <@ doc)
        sArtist = toString $ firstAttr "name"
          (isTagOpenName "tumblelog" <@ doc)
        sPageUrl = toString $ firstAttr "url-with-slug"
          (isTagOpenName "post" <@ doc)

postRegex :: String
postRegex = "\\A(https?://.+\\.tumblr\\.com)/(?:post|image)/(\\d+)"
