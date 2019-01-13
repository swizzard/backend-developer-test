{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Game.GameType where

import Control.Lens.TH
import Data.Aeson
import Data.Aeson.TH
import Data.Ord (comparing)
import Data.String (IsString)
import Data.Text (Text)
import GHC.Generics

import AesonHelpers (aOpts)

newtype GameTypeName = GameTypeName { unGameTypeName :: Text}
  deriving (Eq, Generic, IsString, Ord, Show, FromJSON, ToJSON)

-- | type of game, e.g. chess, catan, etc.
-- we use 'Int' for '_numPlayers' because
--   * it's not going to be incredibly huge (i.e. > 2^29 - 1)
--   * 'Prelude.length' returns 'Int' and we won't have to convert
data GameType = GameType { _gameTypeName :: GameTypeName ,
                           _numPlayers :: [Int] } deriving (Eq, Generic, Show)

instance Ord GameType where
  compare = comparing (unGameTypeName . _gameTypeName)

deriveJSON aOpts ''GameType
makeLenses ''GameType
