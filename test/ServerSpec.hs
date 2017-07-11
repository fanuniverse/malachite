{-# LANGUAGE QuasiQuotes #-}

module ServerSpec where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Network.Wai (Application)

import Server (ServerConfiguration(..), app)

main :: IO ()
main = hspec spec

testApp :: IO Application
testApp = return $ app $ ServerConfiguration
  { camoHost = "https://camo.org"
  , camoKey = "461fbf74af826c3a1020"
  , port = 3000 }

spec :: Spec
spec = with testApp $ do
  describe "GET /?url=" $ do
    context "(supported url)" $ do
      it "responds with JSON-encoded scraped data" $ do
        get "/?url=https://www.example.com/path/to/image.png"
          `shouldRespondWith`
            [json| { imageUrl: "https://www.example.com/path/to/image.png",
                     thumbnailUrl: "https://camo.org/b82e650cfe20239b66f7165e54c3b9036d722aef/68747470733a2f2f7777772e6578616d706c652e636f6d2f706174682f746f2f696d6167652e706e67",
                     artist: null,
                     pageUrl: null } |]
            { matchStatus = 200
            , matchHeaders = ["Content-Type" <:> "application/json"] }

    context "(unsupported or invalid url)" $ do
      it "responds with Not Found" $ do
        get "/?url=https://www.someobscurefanartsite.org/images/56"
          `shouldRespondWith` 404

  describe "GET /" $ do
    it "responds with Bad Request" $ do
      get "/" `shouldRespondWith` 400
