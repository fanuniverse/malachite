{-# LANGUAGE ScopedTypeVariables #-}

module Scraper.Deviantart (fromPost, fromCDN) where

import Scraper
import Scraper.Internal

import Strings (BString, toString)

import Text.Regex.PCRE ((=~))

import Network.HTTP.Conduit (Cookie)

import Text.HTML.TagSoup.Fast (parseTags)
import Text.HTML.TagSoup (isTagOpenName, fromAttrib)

import Control.Arrow (second)
import Data.List (find)

fromPost :: (MonadHTTP m) => String -> m (Maybe Scraped)
fromPost url = go =<< (second parseTags <$> fetchPage url [])
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
        downloadUrl = (fromAttrib "href") <$>
          find (hasClass "dev-page-download") pageLinks
        imagePreviewUrl = toString $ firstAttr "src"
          (hasClass "dev-content-full" <@ page)
        sThumbnailUrl = toString $ firstAttr "src"
          (hasClass "dev-content-normal" <@ page)
        sArtist = toString $ firstText
          (hasClass "username" <@ hasClass "dev-title-container" <@ page)
        sCanonicalUrl = toString $ firstAttr "content"
          (hasAttr "property" "og:url" <@ page)
        pageLinks = filter (isTagOpenName "a") page

fromCDN :: (MonadHTTP m) => String -> m (Maybe Scraped)
fromCDN cdnUrl =
  case cdnUrl =~ ("-(.+)\\..+\\z" :: String) of
    [[_, favMeCode]] ->
      fromPost =<< redirectedFrom ("http://fav.me/" ++ favMeCode)
    _ ->
      return Nothing

followUrl :: (MonadHTTP m) => BString -> [Cookie] -> m String
followUrl url cookies =
  redirectedFromWithCookies (toString url) cookies
