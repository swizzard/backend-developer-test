{-# LANGUAGE OverloadedStrings #-}
module Fakes where

import qualified Data.Set as S


import Types.Game.GameType (GameType(..), GameTypeName)
import Types.Location (Location(..))
import Types.User (Age(..), User(..), UserName)

l1 :: Location
l1 = Location "Austin, TX, USA" 30.267 (-97.743)

l2 :: Location
l2 = Location "London, England" 51.507 (-0.128)

l3 :: Location
l3 = Location "Plano, TX, USA" 33.020 (-96.698)

l4 :: Location
l4 = Location "Cambridge, England" 52.203 0.131

l5 :: Location
l5 = Location "Cambridge, MA, USA" 42.365 (-71.105)


chess :: GameType
chess = GameType "Chess" [2]

catan :: GameType
catan = GameType "Settlers of Catan" [3,4]

risk :: GameType
risk = GameType "Risk" [3,4,5]


u1 :: User
u1 = User "abc123" "User One" (Age 20) l1
          (S.fromList ["Chess", "Settlers of Catan", "Risk"])
          True

u2 :: User
u2 = User "def456" "User Two" (Age 33) l2
          (S.fromList ["Chess", "Risk"])
          False

u3 :: User
u3 = User "ghi789" "User Three" (Age 40) l3
          (S.fromList ["Settlers of Catan"])
          True

u4 :: User
u4 = User "jkl012" "User Four" (Age 15) l5
          (S.fromList ["Settlers of Catan", "Risk"])
          False
