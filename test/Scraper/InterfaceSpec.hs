module Scraper.InterfaceSpec where

import Test.Hspec
import HTTPCassette

import Scraper (Scraped(..))
import Scraper.Interface

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "direct image urls" $ do
    it "returns direct image urls without following them" $ (playCassette . scrape)
      "https://example.com/some/path/image.png" >>= \scraped ->
        scraped `shouldBe` Just Scraped
          { imageUrl = "https://example.com/some/path/image.png"
          , thumbnailUrl = "https://example.com/some/path/image.png"
          , artist = Nothing
          , pageUrl = Nothing }

  describe "urls without protocol specified" $ do
    it "assumes HTTP" $ (playCassette . scrape)
      "example.com/some/path/image.png" >>= \scraped ->
        scraped `shouldBe` Just Scraped
          { imageUrl = "http://example.com/some/path/image.png"
          , thumbnailUrl = "http://example.com/some/path/image.png"
          , artist = Nothing
          , pageUrl = Nothing }

  describe "unsupported urls" $ do
    it "returns Nothing" $ (playCassette . scrape)
      "https://www.someobscurefanartsite.org/images/56" >>= \scraped ->
        scraped `shouldBe` Nothing
