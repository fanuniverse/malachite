module Scraper.TumblrSpec where

import Test.Hspec
import HTTPCassette

import Scraper (Scraped(..))
import Scraper.Interface

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "tumblr scraping" $ do
    it "handles /post links" $ (playCassette . scrape)
      "https://iam-photography.tumblr.com/post/157409807702/crescent-over-the-flame" >>= \scraped ->
        scraped `shouldBe` Just Scraped
          { imageUrl = "http://media.tumblr.com/f68ccec85fba7442ab715691d4d07615/tumblr_oldh8iK0kd1ubet1po1_raw.jpg"
          , thumbnailUrl = "https://68.media.tumblr.com/f68ccec85fba7442ab715691d4d07615/tumblr_oldh8iK0kd1ubet1po1_500.jpg"
          , artist = Just "iam-photography"
          , pageUrl = Just "http://iam-photography.tumblr.com/post/157409807702/crescent-over-the-flame" }

    it "handles /post links without slugs" $ (playCassette . scrape)
      "http://curt0s.tumblr.com/post/160725187695?something#something" >>= \scraped ->
        scraped `shouldBe` Just Scraped
          { imageUrl = "http://media.tumblr.com/93d8ad60881c463d7492e8e78d763dab/tumblr_oq1cb6yTZG1tqglaio1_raw.jpg"
          , thumbnailUrl = "http://68.media.tumblr.com/93d8ad60881c463d7492e8e78d763dab/tumblr_oq1cb6yTZG1tqglaio1_500.jpg"
          , artist = Just "curt0s"
          , pageUrl = Just "http://curt0s.tumblr.com/post/160725187695" }

    it "handles /image links" $ (playCassette . scrape)
      "http://euph0r14.tumblr.com/image/152986865194" >>= \scraped ->
        scraped `shouldBe` Just Scraped
          { imageUrl = "http://media.tumblr.com/e7184fec946ac5726c357a8358ed0205/tumblr_ogf02thEGV1qh41oao1_raw.jpg"
          , thumbnailUrl = "http://68.media.tumblr.com/e7184fec946ac5726c357a8358ed0205/tumblr_ogf02thEGV1qh41oao1_500.jpg"
          , artist = Just "euph0r14"
          , pageUrl = Just "http://euph0r14.tumblr.com/post/152986865194/landscape-ripples-by-daniel-photo" }

    it "handles posts with raw image unavailable" $ (playCassette . scrape)
      "https://staff.tumblr.com/post/820098218/norad" >>= \scraped ->
        scraped `shouldBe` Just Scraped
          { imageUrl = "https://68.media.tumblr.com/tumblr_l5nv2u80Br1qz8q0ho1_500.png"
          , thumbnailUrl = "https://68.media.tumblr.com/tumblr_l5nv2u80Br1qz8q0ho1_500.png"
          , artist = Just "staff"
          , pageUrl = Just "https://staff.tumblr.com/post/820098218/norad" }
