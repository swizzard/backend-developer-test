{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
module App where

import Control.Lens
import Control.Lens.TH
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.Aeson.TH
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T
import GHC.Generics
import Servant
import Servant.Server
import System.Envy hiding ((.=))

import AesonHelpers (aOpts)
import DB.Helpers
import DB
import Location
import Types.Game.GameType
import Types.User


data AppState = AppState {
                conn :: PGInfo
              , mqi :: MQInfo
              } deriving (Show)

newAppState :: IO (Either String AppState)
newAppState = do
  ci <- getInfo
  mi <- getMQI
  return $ AppState <$> ci <*> mi

type AppM = ReaderT AppState Handler

-- for now
type GamesAPI = EmptyAPI

api :: Proxy GamesAPI
api = Proxy

server :: ServerT GamesAPI AppM
server = undefined


data UserCredentials = UserCredentials {
                       _signInEmail :: Email
                     , _signInToken :: GToken
                     } deriving (Eq, Generic, Show)

deriveJSON aOpts ''UserCredentials
makeLenses ''UserCredentials

type UserSignIn = "sign-in" :> ReqBody '[JSON] UserCredentials :> Post '[JSON] User

type CreateUser = ReqBody '[JSON] NewUser :> Post '[JSON] User

type AddInterest = "interests" :> Capture "userid" Integer :> Capture "gametypename" T.Text :> PostCreated '[JSON] NoContent

type Near = "near" :> Capture "userid" Integer :> Get '[JSON] User

type UserApi = "users" :> UserSignIn :<|> CreateUser :<|> AddInterest :<|> Near

signIn :: UserCredentials -> AppM User
signIn (UserCredentials email token) = do
  pgi <- asks conn
  signInResp <- liftIO $ userSignInQ pgi email token
  case signInResp of
    (Left e) -> throwError $ err404 { errBody = (LB.pack e) }
    (Right u) -> return u

createUser :: NewUser -> AppM User
createUser nu = do
  pgi <- asks conn
  mq <- asks mqi
  newUserResp <- liftIO $ newUserQ pgi mq nu
  case newUserResp of
    (Left e) -> throwError $ err400 { errBody = (LB.pack e) }
    (Right u) -> return u

addInterest :: Int -> T.Text -> AppM ()
addInterest uid gtName = do
  pgi <- asks conn
  gtIdResp <- liftIO $ gameTypeIdQ pgi (GameTypeName gtName)
  case gtIdResp of
    (Left e) -> throwError $ err404 { errBody = (LB.pack e) }
    (Right gtid) -> liftIO $ postInterestQ pgi uid gtid

-- nearMe :: Int -> AppM [User]
-- nearMe uid = do
--   pgi <- asks conn
--   interestsR <-
