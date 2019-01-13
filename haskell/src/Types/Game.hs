{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Game where

import Control.Arrow
import Control.Lens
import Control.Lens.TH
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics

import AesonHelpers (aOpts)
import Misc (arrc)
import Types.Game.GameType (GameType, numPlayers)
import Types.User (User)

newtype GameName = GameName { unGameName :: Text } deriving (Eq, Generic, IsString,
                                                             Show, FromJSON, ToJSON)


-- | instance of a game (waiting to be played or being played)
data GameInstance = GameInstance {
                    _gType :: GameType
                  , _host :: User
                  , _players :: Set User
                  , _gameName :: GameName
                  , _active :: Bool
                  } deriving (Eq, Generic, Show)

deriveJSON aOpts ''GameInstance
makeLenses ''GameInstance

-- | check if a 'GameInstance' is not active (being played)
notActive :: GameInstance -> Bool
notActive = views active not

-- | check if the number of 'User's in a 'GameInstance' is less than the maximum allowed
-- by its 'GameType'
playersUnderCap :: GameInstance -> Bool
playersUnderCap = arrc (<=) currPlayers maxPlayers where
  currPlayers = views players length
  maxPlayers = views (gType . numPlayers) (foldl1 max)

-- | check if a 'GameInstance' can be joined, i.e. it has room for more players and
-- isn't currently being played
joinable :: GameInstance -> Bool
joinable = arrc (&&) notActive playersUnderCap
