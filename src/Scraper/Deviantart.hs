{-# LANGUAGE ScopedTypeVariables #-}

module Scraper.Deviantart (fromPost, fromCDN) where

import Scraper
import Scraper.Internal

import Strings (BString, bString, toString)

import Text.Regex.PCRE ((=~))

import Network.HTTP.Conduit (Cookie)

import Text.HTML.TagSoup.Fast (parseTags)
import Text.HTML.TagSoup (isTagOpenName, fromAttrib)

import Data.List (find)

fromPost :: (MonadHTTP m) => String -> m (Maybe Scraped)
fromPost url = do
  (cookies, body) <- fetchPage url []
  let page = parseTags body
      pageLinks = filter (isTagOpenName "a") page
      downloadLink = find (hasClass "dev-page-download") pageLinks
  sImageUrl <- case downloadLink of
    Just link -> followDownloadLink (fromAttrib "href" link) cookies
    Nothing -> return $ firstAttr "src" $ hasClass "dev-content-full" <@ page
  let sThumbnailUrl = firstAttr "src" $ hasClass "dev-content-normal" <@ page
      sArtist = firstText $ hasClass "username" <@ hasClass "dev-title-container" <@ page
      sPageUrl = firstAttr "content" $ (hasAttr "property" "og:url") <@ page
  return $ Just Scraped { imageUrl = toString sImageUrl
                        , thumbnailUrl = toString sThumbnailUrl
                        , artist = Just $ toString sArtist
                        , pageUrl = Just $ toString sPageUrl }

fromCDN :: (MonadHTTP m) => String -> m (Maybe Scraped)
fromCDN cdnUrl =
  case favMeUrl of
    Just url -> fromPost =<< redirectedFrom url
    Nothing -> return Nothing
  where
    favMeUrl = ("http://fav.me/" ++) <$> favMeCode
    favMeCode =
      case cdnUrl =~ ("-(.+)\\..+\\z" :: String) of
        [[_, code]] -> Just code
        _ -> Nothing

followDownloadLink :: (MonadHTTP m) => BString -> [Cookie] -> m BString
followDownloadLink link cookies =
  bString <$> redirectedFromWithCookies (toString link) cookies
