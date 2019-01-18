{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.User where

import Control.Lens
import Control.Lens.TH
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

import AesonHelpers (aOpts)
import Types.Location
import Types.Game.GameType (GameTypeName)

newtype Age = Age { unAge:: Integer } deriving (Eq, Generic, Show, FromJSON, ToJSON)

newtype Email = Email { unEmail :: Text }
  deriving (Eq, Generic, IsString, Show, FromJSON, ToJSON)

newtype UserName = UserName { unUserName :: Text }
  deriving (Eq, Generic, Show, IsString, FromJSON, ToJSON)

newtype GToken = GToken { unGToken :: Text }
  deriving (Eq, Generic, IsString, Show, FromJSON, ToJSON)

data User = User {
            _id :: Int
          , _email :: Email
          , _token :: GToken
          , _userName :: UserName
          , _age :: Age
          , _location :: Location
          , _interestedIn :: Set GameTypeName
          , _canHost :: Bool
          } deriving (Eq, Generic, Show)

instance ToJSON User where
  toEncoding = A.genericToEncoding aOpts

instance FromJSON User where
  parseJSON = A.withObject "User" $ \v -> do
    pk <- v A..: "id"
    em <- v A..: "email"
    tk <- v A..: "token"
    un <- v A..: "username"
    ag <- v A..: "age"
    lo <- v A..: "location"
    ii <- v A..:? "interestedin" A..!= S.empty
    ch <- v A..: "canhost"
    return $ User pk em tk un ag lo ii ch


instance Ord User where
  compare = comparing _id

makeLenses ''User


-- | helper function to add a 'Types.Game.GameType' to a 'User'\'s interests
addInterest :: User -> GameTypeName -> User
addInterest u gt = u & interestedIn %~ (S.insert gt)

-- | helper function to remove a 'Types.Game.GameType' from a 'User'\'s interests
removeInterest :: User -> GameTypeName -> User
removeInterest u gt = u & interestedIn %~ (S.delete gt)

-- | get other 'User's interested in the same 'Types.Game.GameType's, sorted by distance
getInterestedIn :: User -> [User] -> Map GameTypeName [User]
getInterestedIn u us = (flip M.restrictKeys) (u ^. interestedIn) $
  M.unionsWith f (userToInterests <$> (filter (not . (== u)) us))
    where f l1 l2 = sortByDist (u ^. location) $ l1 ++ l2

-- | helper function that transforms a 'User' into a map keyed by their interests
userToInterests :: User -> Map GameTypeName [User]
userToInterests u = u ^. interestedIn & M.fromSet (const [u])

-- | sort a list of 'User's by their distance from a given location
sortByDist :: Location -> [User] -> [User]
sortByDist l = sortBy (comparing (views location (distanceFrom l)))


data NewUser = NewUser {
               _nEmail :: Email
             , _nToken :: GToken
             , _nUserName :: UserName
             , _nAge :: Age
             , _nLocationName :: LocationName
             , _nCanHost :: Bool
             } deriving (Eq, Generic, Show)

makeLenses ''NewUser

instance ToJSON NewUser where
  toJSON NewUser{..} = A.object ["email" A..= _nEmail, "token" A..= _nToken,
                                "username" A..= _nUserName, "age" A..= _nAge,
                                "location" A..= _nLocationName, "canhost" A..= _nCanHost]
