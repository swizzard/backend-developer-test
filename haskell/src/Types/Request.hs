{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Types.Request where

import Control.Lens
import Control.Lens.TH
import Data.Aeson.TH
import GHC.Generics

import AesonHelpers (aOpts)
import Types.Game (GameInstance, gType, joinable, players)
import Types.Game.GameType (gameTypeName)
import Types.User (User, interestedIn)


data Invite = Invite {
              _inviter :: User
            , _invitee :: User
            , _invGame :: GameInstance
            , _read :: Bool
            , _accepted :: Bool
            } deriving (Eq, Generic, Show)

deriveJSON aOpts ''Invite
makeLenses ''Invite


-- | send an invite from a player waiting to play a game to a player who's interested
-- currently working under the assumption that any player can make an invite, not just
-- the host
makeInvite :: User -> GameInstance -> User -> Maybe Invite
makeInvite er g ee = if joinable g && er `isPlaying` g && g `interests` ee
                        then Just $ Invite er ee g False False
                        else Nothing

-- | is a user playing (or waiting to play) a game?
isPlaying :: User -> GameInstance -> Bool
isPlaying u g = views players (u `elem`) g

-- | is a user interested in playing a given type of game?
interests :: GameInstance -> User -> Bool
interests g u = views interestedIn ((g ^. gType . gameTypeName) `elem`) u
