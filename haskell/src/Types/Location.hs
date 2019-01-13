{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Location where

import Control.Lens.TH
import Data.Aeson
import Data.Aeson.TH
import Data.Ord (comparing)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics

import AesonHelpers (aOpts)


newtype LocationName = LocationName { unLocationName :: Text }
  deriving (Eq, Generic, IsString, Show, FromJSON, ToJSON)

data Location = Location {
                _locationName :: LocationName
              , _lat :: Double
              , _lon :: Double
              } deriving (Eq, Generic, Show)

deriveJSON aOpts ''Location
makeLenses ''Location

instance Ord Location where
  compare = comparing (unLocationName . _locationName)


-- | calculate the distance between two 'Location's
distanceFrom :: Location -> Location -> Double
distanceFrom (Location _ la1 lo1) (Location _ la2 lo2) = sqrt $ (la1 - la2) + (lo1 - lo2)
