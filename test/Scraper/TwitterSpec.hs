module Scraper.TwitterSpec where

import Test.Hspec
import HTTPCassette

import Scraper (Scraped(..))
import Scraper.Interface

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "twitter scraping" $ do
    it "handles /status links with images" $ (playCassette . scrape)
      "https://www.twitter.com/matt_forster/status/885468722680475649" >>= \scraped ->
        scraped `shouldBe` Just Scraped
          { imageUrl = "https://pbs.twimg.com/media/DEnRGUpWAAQ_aBK.jpg:orig"
          , thumbnailUrl = "https://pbs.twimg.com/media/DEnRGUpWAAQ_aBK.jpg:small"
          , artist = Just "Matt_Forster"
          , pageUrl = Just "https://twitter.com/Matt_Forster/status/885468722680475649" }
