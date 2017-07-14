{-# LANGUAGE ScopedTypeVariables #-}

module Scraper.Deviantart (scrapePost, scrapeCDN) where

import Scraper
import Scraper.Internal

import Data.Text (Text)
import Data.List (find)
import Network.HTTP.Conduit (Cookie)
import Text.HTML.TagSoup (isTagOpenName, fromAttrib)
import qualified Data.Text as Text

scrapePost :: (MonadHTTP m) => String -> m (Maybe Scraped)
scrapePost url = go =<< fetchPage url []
  where
    go (cookies, page) = do
      sImageUrl <- case downloadUrl of
        Just download -> followUrl download cookies
        Nothing -> pure imagePreviewUrl
      return $ Just Scraped { imageUrl = sImageUrl
                            , thumbnailUrl = sThumbnailUrl
                            , artist = Just sArtist
                            , pageUrl = Just sCanonicalUrl }
      where
        downloadUrl = fromAttrib "href" <$>
          find (hasClass "dev-page-download") pageLinks
        imagePreviewUrl = firstAttr "src"
          (hasClass "dev-content-full" <@ page)
        sThumbnailUrl = firstAttr "src"
          (hasClass "dev-content-normal" <@ page)
        sArtist = firstText
          (hasClass "username" <@ hasClass "dev-title-container" <@ page)
        sCanonicalUrl = firstMetaContent "og:url" page
        pageLinks = filter (isTagOpenName "a") page

scrapeCDN :: (MonadHTTP m) => String -> m (Maybe Scraped)
scrapeCDN cdnUrl =
  case cdnUrl =~ ("-(.+)\\..+\\z" :: String) of
    [[_, favMeCode]] ->
      scrapePost =<< redirectedFrom ("http://fav.me/" ++ favMeCode)
    _ ->
      return Nothing

followUrl :: (MonadHTTP m) => Text -> [Cookie] -> m Text
followUrl url cookies = Text.pack <$>
  redirectedFromWithCookies (Text.unpack url) cookies
