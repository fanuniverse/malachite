module Main where

import Server (ServerConfiguration(..), runServer)

import System.Environment (getEnv)

main :: IO ()
main = do
  envCamoHost <- getEnv "CAMO_HOST"
  envCamoKey <- getEnv "CAMO_KEY"
  envPort <- getEnv "PORT"
  runServer $ ServerConfiguration
    { camoHost = envCamoHost
    , camoKey = envCamoKey
    , port = read envPort }
