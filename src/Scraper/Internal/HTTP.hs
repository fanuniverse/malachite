module Scraper.Internal.HTTP ( httpUrl
                             , fetchPage
                             , reqStatus
                             , redirectedFrom
                             , redirectedFromWithCookies) where

import Scraper.Internal.MonadHTTP (MonadHTTP, LResponse, httpLResponse)

import Data.List (find)
import Data.Char (isSpace)
import Data.Text (Text)

import Text.Regex.PCRE ((=~))
import Text.HTML.TagSoup (Tag)
import Text.HTML.TagSoup.Fast (parseTagsT)

import Network.HTTP.Conduit
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Status (statusCode)

import Data.ByteString.UTF8 (toString)
import qualified Data.ByteString.Lazy as L

urlScheme :: String -> String
urlScheme url = url =~ ("\\A(.+)://" :: String)

httpUrl :: String -> Maybe String
httpUrl url = if all isSpace url
  then Nothing
  else case urlScheme url of
         "https://" -> Just url
         "http://"  -> Just url
         ""         -> Just ("http://" ++ url)
         _          -> Nothing

userAgent :: Header
userAgent = ("User-Agent", "Mozilla/5.0 (Linux x86_64) Malachite/1.0")

-- Low-level HTTP fetch function.
-- Request modifier function -> URL ->  headers -> cookies
-- Headers should _not_ contain "User-Agent".
fetch :: (MonadHTTP m) =>
  (Request -> Request) -> String -> [Header] -> [Cookie] -> m LResponse
fetch fmod url headers cookies = do
  initialRequest <- parseRequest url
  let request = (fmod initialRequest)
                 { requestHeaders = userAgent : headers
                 , cookieJar = Just $ createCookieJar cookies
                 }
  httpLResponse request

-- Higher-level HTTP GET function.
-- URL -> cookies -> a tuple of (cookies, parsed web page)
fetchPage :: (MonadHTTP m) => String -> [Cookie] -> m ([Cookie], [Tag Text])
fetchPage url reqCookies = go <$> fetch request url [] reqCookies
  where
    request r = r { method = "GET" }
    go response = (cookies, page)
      where
        cookies = (destroyCookieJar . responseCookieJar) response
        page = (parseTagsT . L.toStrict . responseBody) response

reqStatus :: (MonadHTTP m) => String -> m Int
reqStatus url = getStatus <$> fetch request url [] []
  where
    getStatus = statusCode . responseStatus
    request r = r { method = "HEAD" }

redirectedFrom :: (MonadHTTP m) => String -> m String
redirectedFrom = flip redirectedFromWithCookies []

redirectedFromWithCookies :: (MonadHTTP m) => String -> [Cookie] -> m String
redirectedFromWithCookies url cookies = do
  response <- fetch request url [] cookies
  let headers = responseHeaders response
      location = find ((== "location") . fst) headers
      newUrl = case location of
        Just (_, loc) -> toString loc
        Nothing -> url
  return newUrl
  where request r = r { method = "HEAD"
                      , redirectCount = 0 }
