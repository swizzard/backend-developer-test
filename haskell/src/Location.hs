{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Location where

import Control.Lens
import Control.Lens.TH
import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.Aeson.Lens
import Data.Maybe (isJust)
import qualified Data.Text as T
import Network.Wreq
import System.Envy

import Types.Location

data MQInfo = MQInfo {
              _mqKey :: T.Text
            , _mqUrl :: String
            } deriving (Show)

instance FromEnv MQInfo where
  fromEnv = MQInfo <$> env "MQ_KEY" <*> pure "http://www.mapquestapi.com/geocoding/v1/address"

makeLenses ''MQInfo

getMQI :: IO (Either String MQInfo)
getMQI = decodeEnv

data LatLng = LatLng { lat :: Double
                     , lng :: Double
                     } deriving (Eq, Show)

deriveJSON defaultOptions ''LatLng

llToLocation :: T.Text -> LatLng -> Location
llToLocation name (LatLng lat lon) = Location (LocationName name) lat lon


getLocation :: MQInfo -> T.Text -> IO (Maybe Location)
getLocation mqi addr = let opts = defaults & param "key" .~ [mqi ^. mqKey]
                           payload = A.encode . A.object $
                                      [("location", A.String addr),
                                       ("thumbMaps", A.Bool False)]
                      in do
                        r <- postWith opts (mqi ^. mqUrl) payload
                        return $ r ^. responseBody
                              ^? key "results"
                               . nth 0
                               . key "locations"
                               . nth 0
                               . key "latLng"
                               . _JSON <&> llToLocation addr
