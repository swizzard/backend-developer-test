{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.User where

import Control.Lens
import Control.Lens.TH
import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:))
import qualified Data.Aeson as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
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
import Types.Location (Location, distanceFrom)
import Types.Game.GameType (GameTypeName)

newtype Age = Age { unAge:: Integer } deriving (Eq, Generic, Show, FromJSON, ToJSON)

-- | GAPI auth token
newtype Token = Token { unToken :: ByteString } deriving (Eq, Generic, IsString, Show)

newtype UserName = UserName { unUserName :: Text }
  deriving (Eq, Generic, Show, IsString, FromJSON, ToJSON)

data User = User {
            _token :: Token
          , _userName :: UserName
          , _age :: Age
          , _location :: Location
          , _interestedIn :: Set GameTypeName
          , _canHost :: Bool
          } deriving (Eq, Generic, Show)

instance ToJSON User where
  toJSON User{..} = object [ "token" A..= B.unpack (unToken _token)
                          , "userName" A..= toJSON _userName
                          , "age" A..= toJSON _age
                          , "location" A..= toJSON _location
                          , "interestedIn" A..= toJSON _interestedIn
                          , "canHost" A..= _canHost
                          ]

instance FromJSON User where
  parseJSON = withObject "User" $ \v -> do
    tk <- v .: "token" >>= parseJSON
    uname <- v .: "userName" >>= parseJSON
    loc <- v .: "location" >>= parseJSON
    age' <- v .: "age"
    int <- v .: "interests" >>= parseJSON
    ch <- v .: "canHost"
    return $ User (Token $ B.pack tk) uname age' loc int ch

instance Ord User where
  compare = comparing (unToken . _token)

makeLenses ''User


-- | helper function to make 'User's
makeUser :: ByteString -> Text -> Integer -> Bool -> Location -> User
makeUser tk name age ch loc = User (Token tk) (UserName name) (Age age)
                                   loc S.empty ch

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
