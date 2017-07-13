{-# LANGUAGE ScopedTypeVariables #-}

module Scraper.Deviantart (fromPost, fromCDN) where

import Scraper
import Scraper.Internal

import Network.HTTP.Conduit (Cookie)

import Text.Regex.PCRE ((=~))
import Text.HTML.TagSoup (isTagOpenName, fromAttrib)

import Data.Text (Text)
import Data.List (find)

import qualified Data.Text as Text

fromPost :: (MonadHTTP m) => String -> m (Maybe Scraped)
fromPost url = go =<< fetchPage url []
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
        imagePreviewUrl = firstAttr "src"
          (hasClass "dev-content-full" <@ page)
        sThumbnailUrl = firstAttr "src"
          (hasClass "dev-content-normal" <@ page)
        sArtist = firstText
          (hasClass "username" <@ hasClass "dev-title-container" <@ page)
        sCanonicalUrl = firstAttr "content"
          (hasAttr "property" "og:url" <@ page)
        pageLinks = filter (isTagOpenName "a") page

fromCDN :: (MonadHTTP m) => String -> m (Maybe Scraped)
fromCDN cdnUrl =
  case cdnUrl =~ ("-(.+)\\..+\\z" :: String) of
    [[_, favMeCode]] ->
      fromPost =<< redirectedFrom ("http://fav.me/" ++ favMeCode)
    _ ->
      return Nothing

followUrl :: (MonadHTTP m) => Text -> [Cookie] -> m Text
followUrl url cookies = Text.pack <$>
  redirectedFromWithCookies (Text.unpack url) cookies
