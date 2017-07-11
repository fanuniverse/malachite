module Scraper.DeviantartSpec where

import Test.Hspec
import HTTPCassette

import Scraper (Scraped(..))
import Scraper.Interface

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "deviantart scraping" $ do
    it "handles posts with downloading disabled" $ (playCassette . scrape)
      "http://oo-rein-oo.deviantart.com/art/Heart-Of-The-City-303912988?some-unrelated-parameter=5" >>= \scraped ->
        scraped `shouldBe` Just Scraped
          { imageUrl = "http://orig02.deviantart.net/b72b/f/2012/144/e/1/e18e380d65641c6e57a6545b33b7a6de-d50xwrg.jpg"
          , thumbnailUrl = "http://orig02.deviantart.net/b72b/f/2012/144/e/1/e18e380d65641c6e57a6545b33b7a6de-d50xwrg.jpg"
          , artist = Just "oO-Rein-Oo"
          , pageUrl = Just "http://oo-rein-oo.deviantart.com/art/Heart-Of-The-City-303912988" }

    it "handles direct links to deviantart cdn" $ (playCassette . scrape)
      "http://img11.deviantart.net/0d5d/i/2013/206/5/c/part_of_your_no_by_tsaoshin-d6f5s39.png" >>= \scraped ->
        scraped `shouldBe` Just Scraped
          { imageUrl = "http://orig04.deviantart.net/2183/f/2013/206/9/b/part_of_your_no_by_tsaoshin-d6f5s39.png"
          , thumbnailUrl = "http://pre04.deviantart.net/e5d6/th/pre/i/2013/206/5/c/part_of_your_no_by_tsaoshin-d6f5s39.png"
          , artist = Just "TsaoShin"
          , pageUrl = Just "http://tsaoshin.deviantart.com/art/Part-of-Your-No-388260981" }

    it "handles fav.me links" $ (playCassette . scrape)
      "http://fav.me/daq0vmo" >>= \scraped ->
        scraped `shouldBe` Just Scraped
          { imageUrl = "http://orig06.deviantart.net/b74a/f/2016/333/b/d/city_by_hangmoon-daq0vmo.jpg"
          , thumbnailUrl = "http://pre14.deviantart.net/5b08/th/pre/i/2016/333/2/3/city_by_hangmoon-daq0vmo.jpg"
          , artist = Just "Hangmoon"
          , pageUrl = Just "http://hangmoon.deviantart.com/art/City-648372768" }
