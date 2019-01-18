module AesonHelpers where

import Data.Aeson (Options(..), defaultOptions)
import Data.Char (toLower)

aOpts = defaultOptions { fieldLabelModifier = (map toLower) . strip_ }
strip_ :: String -> String
strip_ ('_':s) = s
strip_ s = s
