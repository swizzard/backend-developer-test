module AesonHelpers where

import Data.Aeson (Options(..), defaultOptions)

aOpts = defaultOptions { fieldLabelModifier = strip_ }
strip_ :: String -> String
strip_ ('_':s) = s
strip_ s = s
