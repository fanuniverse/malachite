module Scraper.Tumblr (fromPost) where

import Scraper
import Scraper.Internal

import Strings (toString)

import Text.Regex.PCRE ((=~))

import Text.HTML.TagSoup.Fast (parseTags)
import Text.HTML.TagSoup (isTagOpenName)

postRegex :: String
postRegex = "\\A(https?://.+\\.tumblr\\.com)/(post|image)/(\\d+)"

fromPost :: (MonadHTTP m) => String -> m (Maybe Scraped)
fromPost url = do
  doc <- parseTags . snd <$> fetchPage apiUrl []
  let sImageUrl = firstText $ hasAttr "max-width" "1280" <@ doc
      sThumbnailUrl = firstText $ hasAttr "max-width" "500" <@ doc
      sArtist = firstAttr "name" $ isTagOpenName "tumblelog" <@ doc
      sPageUrl = firstAttr "url-with-slug" $ isTagOpenName "post" <@ doc
  return $ Just Scraped { imageUrl = toString sImageUrl
                        , thumbnailUrl = toString sThumbnailUrl
                        , artist = Just $ toString sArtist
                        , pageUrl = Just $ toString sPageUrl }
  where apiUrl = blog ++ "/api/read?id=" ++ postId
        (blog, postId) = (\m -> ((m !! 1), (m !! 3))) . head $ (url =~ postRegex :: [[String]])
