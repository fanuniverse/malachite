module Main where

import Server (ServerConfiguration(..), runServer)

import qualified Data.Text as Text
import System.Environment (getEnv)

main :: IO ()
main = do
  envCamoHost <- getEnv "CAMO_HOST"
  envCamoKey <- getEnv "CAMO_KEY"
  envPort <- getEnv "PORT"
  runServer $ ServerConfiguration
    { camoHost = Text.pack $ envCamoHost
    , camoKey = Text.pack $ envCamoKey
    , port = read envPort }
