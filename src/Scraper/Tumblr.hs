module Scraper.Tumblr (scrapePost) where

import Scraper
import Scraper.Internal

import Text.Regex.PCRE ((=~))
import Text.HTML.TagSoup (isTagOpenName)
import Data.Text (Text)
import qualified Data.Text as Text

scrapePost :: (MonadHTTP m) => String -> m (Maybe Scraped)
scrapePost url = go =<< (snd <$> fetchPage apiUrl [])
  where
    apiUrl = blog ++ "/api/read?id=" ++ postId
    [[_, blog, postId]] = url =~ postRegex
    go doc = do
      sImageUrl <- largestImgUrl scaledImageUrl
      return $ Just Scraped
        { imageUrl = sImageUrl
        , thumbnailUrl = sThumbnailUrl
        , artist = Just sArtist
        , pageUrl = Just sPageUrl }
      where
        scaledImageUrl = firstText
          (hasAttr "max-width" "1280" <@ doc)
        sThumbnailUrl = firstText
          (hasAttr "max-width" "500" <@ doc)
        sArtist = firstAttr "name"
          (isTagOpenName "tumblelog" <@ doc)
        sPageUrl = firstAttr "url-with-slug"
          (isTagOpenName "post" <@ doc)

largestImgUrl :: (MonadHTTP m) => Text -> m Text
largestImgUrl scaledUrl = do
  rawAvailable <- ((==) 200) <$> reqStatus rawUrl
  return (if rawAvailable then Text.pack rawUrl else scaledUrl)
  where
    rawUrl = "http://media.tumblr.com/" ++ path ++ "_raw." ++ ext
    [[_, path, ext]] = (Text.unpack scaledUrl) =~ cdnPathRegex

postRegex :: String
postRegex = "\\A(https?://.+\\.tumblr\\.com)/(?:post|image)/(\\d+)"

cdnPathRegex :: String
cdnPathRegex = ".+tumblr[.]com/(.+)_\\d+[.](.+)\\z"
