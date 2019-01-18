{-# LANGUAGE OverloadedStrings #-}
module DB where


import Control.Lens
import Control.Lens.TH
import qualified Data.Aeson as A
import Data.Aeson.Lens
import qualified Data.ByteString as B
import Data.Functor (($>))
import Data.List (nub)
import qualified Data.Text as T
import Network.Wreq

import DB.Helpers
import Location
import Misc (tShow)
import Types.Game
import Types.Game.GameType
import Types.Location
import Types.Request
import Types.User


userData = param "select" .~ ["id,email,token,username,age,canhost," <>
                              "location:locations(*),interestedin:" <>
                              "gametypes(gametypename)"]

-- | get a user, given our root and jwt + the user's email
userSignInQ :: PGInfo -> Email -> GToken -> IO (Either String User)
-- passing the root + jwt explicitly into this function is less than great
-- since realistically it'll be called from within whatever our final StateT monad,
-- but we don't have that nailed down yet so this is good enough.
userSignInQ info (Email email) (GToken tok) = let opts = ro `withBearer` info
                                                    & "email" `eq` email
                                                    & "token" `eq` tok
                                                    & userData & justOne
                                  in getWith opts (info >/ "users") <&> rtj

userInvitesQ :: PGInfo -> Int -> IO [Invite]
userInvitesQ info userId = let opts = ro `withBearer` info
                                        & "id" `eq` (tShow userId)
                                 in getWith opts (info >/ "invites")
                                      <&> rtj <&> either (const []) Prelude.id


newUserQ :: PGInfo -> MQInfo -> NewUser -> IO (Either String User)
newUserQ info mqi newUser = let opts = ro `withBearer` info
                                        & returnRep & justOne
                                nuQ = param "select" .~
                                    ["id,email,token,username,age," <>
                                     "canhost,location:locations(*)"]
                             in do
                               l <- getLocation mqi (newUser ^. nLocationName .
                                                     to unLocationName)
                               case l of
                                 Nothing -> return $ Left "invalid location"
                                 (Just lc) -> do
                                   loc <- postWith opts (info >/ "locations")
                                                        (A.toJSON $ lc)
                                   case (loc ^. responseBody ^? key "id" . _Number) of
                                     Nothing -> return $ Left "invalid location"
                                     (Just locId) -> let nuJS = (A.toJSON newUser)
                                                                   & key "location"
                                                                   .~ A.Number locId
                                                      in postWith (opts & nuQ)
                                                                  (info >/ "users") nuJS
                                                                  <&> rtj

userInterestsQ :: PGInfo -> Int -> IO [GTR]
userInterestsQ info uid = let opts = ro `withBearer` info & "users.id" `eq` (tShow uid)
                                        & param "select" .~ ["gametypes(*)"]
                           in do
                             resp <- getWith opts (info >/ "userinterests")
                             return $ (resp ^. responseBody ^.. values
                                        . key "gametypes" . _JSON :: [GTR])

gameTypesQ :: PGInfo -> IO (Either String [GameType])
gameTypesQ info = getWith (ro `withBearer` info) (info >/ "gametypes") <&> rtj

gameTypeIdQ :: PGInfo -> GameTypeName -> IO (Either String Int)
gameTypeIdQ info (GameTypeName name) = let opts = ro `withBearer` info
                                                   & "gametypename" `eq` name
                                                   & returnRep & justOne
                                                   & param "select" .~ ["id"]
                                        in do
                                          r <- getWith opts (info >/ "gametypes")
                                          let gtid = r ^. responseBody ^?
                                                      key "id" . _Integral
                                          case gtid of
                                            Nothing -> return $ Left "no matching gametype"
                                            (Just pk) -> return $ Right pk

postInterestQ :: PGInfo -> Int -> Int -> IO ()
postInterestQ info uid gtid = postWith (ro `withBearer` info)
                                       (info >/ "userinterests") uiPayload $> ()
   where uiPayload = A.pairs ("user_" A..= uid <> "gametype" A..= gtid)

addInterestQ :: PGInfo -> Int -> GameTypeName -> IO (Either String ())
addInterestQ info userId (GameTypeName gtn) = do
  let gto = ro `withBearer` info & returnRep & justOne & "gametypename" `eq` gtn
  gtr <- getWith gto (info >/ "gametypes")
  case (gtr ^. responseBody ^? key "id" . _Integer . to fromInteger) of
    (Just gtid) -> Right <$> postInterestQ info userId gtid
    Nothing -> return $ Left "no such gametype"

byInterestQ :: PGInfo -> Int -> [GameTypeName] -> IO (Either String [User])
byInterestQ info uid interests = do
   uR <- getWith (bo & justOne & userData & "id" `eq` (tShow uid)) (info >/ "users") <&> rtj
   case uR of
     -- gotta pull it out & repackage it because uR :: IO (Either String User) & we need
     -- to return IO (Either String [User])
     (Left e) -> return $ Left e
     (Right u) -> do
        uidR <- getWith uidOpts (info >/ "userinterets")
        case (nub $ uidR ^. responseBody ^.. values . key "users" . key "id"
              . _Integer . to fromInteger) of
          [] -> return $ Right []
          uids -> do
            us <- getWith (uOpts uids) (info >/ "users") <&> rtj
            case us of
              e@(Left _) -> return e
              (Right uss) -> return $ Right $ sortByDist (u ^. location) uss
  where bo = ro `withBearer` info
        uidOpts = bo & param "gametype.gametypename" .~
                   ["in.(" <> (T.intercalate "," $ unGameTypeName <$> interests) <> ")"]
                   & param "select" .~ ["users(id)"]
        uOpts ids = bo & userData & param "user.id" .~
                      ["in.(" <> (T.intercalate "," $ tShow <$> ids) <> ")"]
