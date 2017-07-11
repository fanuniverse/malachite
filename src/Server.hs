{-# LANGUAGE LambdaCase #-}

module Server (ServerConfiguration(..), runServer, app) where

import Scraper (Scraped)
import Scraper.Interface (scrape)
import Strings (toString)
import Camo (camoifyScraped)

import Control.Monad (join)

import Data.Aeson (encode)

import Network.Wai (Application, Response, queryString, responseLBS)
import Network.Wai.Handler.Warp (Port, run)
import Network.HTTP.Types (ok200, badRequest400, notFound404)
import Network.HTTP.Types.Header (hContentType)

data ServerConfiguration = ServerConfiguration
  { camoHost :: String
  , camoKey  :: String
  , port     :: Port }
  deriving (Show, Eq)

runServer :: ServerConfiguration -> IO ()
runServer config = run serverPort application
  where serverPort = port config
        application = app config

app :: ServerConfiguration -> Application
app config request respond = respond =<<
  let requestedUrl = join $ lookup "url" $ queryString request
  in case requestedUrl of
    Just url -> (respondWithScraped config) <$> (scrape (toString url))
    Nothing -> return respondWith400

respondWithScraped :: ServerConfiguration -> Maybe Scraped -> Response
respondWithScraped config = \case
  Just scraped -> responseLBS ok200 [(hContentType, "application/json")] json
    where json = (encode . camoify) scraped
          camoify = camoifyScraped (camoHost config) (camoKey config)
  Nothing -> responseLBS notFound404 [] ""

respondWith400 :: Response
respondWith400 = responseLBS badRequest400 [] ""
