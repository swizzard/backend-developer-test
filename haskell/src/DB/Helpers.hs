{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module DB.Helpers where

import Control.Exception
import Control.Lens
import Control.Lens.TH
import qualified Data.Aeson as A
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Network.Wreq
import System.Envy

import Misc (tShow, tToB)


data PGInfo = PGInfo {
              _root :: String
            , _jwt :: T.Text
            } deriving (Show)

makeLenses ''PGInfo


instance FromEnv PGInfo where
  fromEnv = do
    ipAddr <- env "PGRST_IP_ADDR"
    port <- env "PGRST_PORT"
    jwt <- env "APP_USER_JWT"
    return $ PGInfo ("http://" ++ ipAddr ++ ":" ++ port ++ "/") jwt


getInfo :: IO (Either String PGInfo)
getInfo = decodeEnv


-- | add the "Bearer:" header we need for PostgREST auth
withBearer :: Options -> PGInfo -> Options
withBearer opts (PGInfo _ tok) = opts & header "Authorization" .~ [tToB $ "Bearer " `T.append` tok]

-- | ensure we try to only get a single response back from PostGREST
-- (see http://postgrest.org/en/v5.2/api.html#singular-or-plural)
justOne :: Options -> Options
justOne = header "Accept" .~ ["application/vnd.pgrst.object+json"]

-- | set the header to get a newly created or updated object back
-- (see http://postgrest.org/en/v5.2/api.html#insertions-updates)
returnRep :: Options -> Options
returnRep = header "Prefer" .~ ["return=representation"]

prm `eq` val = param prm .~ ["eq." `T.append` val]

endpoint :: PGInfo -> String -> String
endpoint pgi s = views root (++ s) pgi

(>/) = endpoint
infixr 7 >/

-- | helper function to convert a JSON response to one of our types
-- 'Network.Wreq.asJSON' already exists but isn't quite what we need--
-- 'IO''s 'MonadThrow' instance just throws a real exception, when what
-- we really want is e.g. 'Maybe'.
rtj :: (A.FromJSON a) => Response (LB.ByteString) -> Either String a
rtj rsp = let status = rsp ^. responseStatus in
              if | (status ^. statusCode) - 200 >= 200 -> Left (status ^.
                                                               statusMessage .
                                                                 to B.unpack)
                 | otherwise -> views responseBody A.eitherDecode' rsp

rc _ _ = return ()

ro = defaults & checkResponse .~ (Just rc)
