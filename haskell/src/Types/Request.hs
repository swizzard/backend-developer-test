{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}


module Types.Request where

import Control.Arrow
import Control.Lens
import Control.Lens.TH
import Data.Aeson (defaultOptions, fieldLabelModifier, genericToEncoding,
                   parseJSON, toEncoding)
import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.Char (toLower)
import qualified Data.Set as S
import GHC.Generics

import AesonHelpers (aOpts)
import Types.Game (GameInstance, gType, joinable, players)
import Types.Game.GameType (gameTypeName)
import Types.User (User, interestedIn)


data InviteState = Unread | Accepted | Ignored | Rejected deriving (Eq, Generic, Show)
instance A.ToJSON InviteState where
  toEncoding = A.genericToEncoding $ A.defaultOptions { A.constructorTagModifier = (map toLower) }

instance A.FromJSON InviteState where
  parseJSON "unread" = return Unread
  parseJSON "accepted" = return Accepted
  parseJSON "ignored" = return Ignored
  parseJSON "rejected" = return Rejected
  parseJSON _ = fail "invalid InviteState"

data Invite = Invite {
              _inviter :: User
            , _invitee :: User
            , _invGame :: GameInstance
            , _invState :: InviteState
            } deriving (Eq, Generic, Show)

deriveJSON aOpts ''Invite
makeLenses ''Invite


-- | send an invite from a player waiting to play a game to a player who's interested.
-- currently working under the assumption that any player can make an invite, not just
-- the host
makeInvite :: User -> GameInstance -> User -> Maybe Invite
makeInvite er g ee = if joinable g && er `isPlaying` g && g `interests` ee
                        then Just $ Invite er ee g Unread
                        else Nothing

-- | is a user playing (or waiting to play) a game?
isPlaying :: User -> GameInstance -> Bool
isPlaying u g = views players (u `elem`) g

-- | is a user interested in playing a given type of game?
interests :: GameInstance -> User -> Bool
interests g u = views interestedIn ((g ^. gType . gameTypeName) `elem`) u

-- | agree to an invite, adding the user to the set of players. Returns the updated
-- invite, which contains the updated game
agreeToInvite :: Invite -> Invite
agreeToInvite inv = inv & (invState .~ Accepted) . (over invGame (\gs -> gs & players %~ (S.insert (inv ^. invitee))))
