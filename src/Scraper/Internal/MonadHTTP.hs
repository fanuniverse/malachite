module Scraper.Internal.MonadHTTP where

import Network.HTTP.Conduit (Request, Response, httpLbs)
import Network.HTTP.Client.TLS (getGlobalManager)

import Control.Monad.Catch (MonadThrow)

import qualified Data.ByteString.Lazy as L

type LResponse = Response L.ByteString

class (Monad m, MonadThrow m) => MonadHTTP m where
  httpLResponse :: Request -> m LResponse

instance MonadHTTP IO where
  httpLResponse r = getGlobalManager >>= httpLbs r
