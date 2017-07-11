module Scraper.Internal.HTTP ( httpUrl
                             , fetchPage
                             , redirectedFrom
                             , redirectedFromWithCookies) where

import Strings (BString, toString)

import Scraper.Internal.MonadHTTP (MonadHTTP, LResponse, httpLResponse)

import Data.List (find)
import Data.Char (isSpace)

import Text.Regex.PCRE ((=~))

import Network.HTTP.Conduit
import Network.HTTP.Types.Header (Header)

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
-- URL -> cookies
fetchPage :: (MonadHTTP m) => String -> [Cookie] -> m ([Cookie], BString)
fetchPage url withCookies = do
  response <- fetch request url [] withCookies
  let cookies = (destroyCookieJar . responseCookieJar) response
      body = (L.toStrict . responseBody) response
  return (cookies, body)
  where request r = r { method = "GET" }

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
