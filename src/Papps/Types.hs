module Papps.Types (
    defaultConfig
  , Config (..)
  , Plugin (..)
  )
  where

data Config = Config {
    configPort :: Int
  , plugins :: [Plugin]
  }

data Plugin = Plugin {
    pluginLanguage :: [String]
  , rawCodeToResult :: String -> String
  }

defaultConfig :: Config
defaultConfig = Config {
    configPort = 5050
  , plugins = []
  }

