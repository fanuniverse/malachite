module Scraper.Internal ( module Scraper.Internal.HTTP
                        , module Scraper.Internal.DOM
                        , MonadHTTP
                        , (=~)
                        , regexMatch
                        ) where

import Scraper.Internal.HTTP
import Scraper.Internal.DOM
import Scraper.Internal.MonadHTTP (MonadHTTP)

import Text.Regex.PCRE ((=~))

import Data.Text (Text)
import qualified Data.Text as Text

regexMatch :: String -> Text -> Text
regexMatch pattern source = Text.pack matchGroup
  where
    [[_, matchGroup]] = (Text.unpack source) =~ pattern
