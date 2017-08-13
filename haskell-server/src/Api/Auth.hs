{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Api.Auth (AuthAPI, authServer) where

import           Config
import           Control.Monad.Reader
import           Crypto.BCrypt
import           Data.Aeson.Types
import           Data.ByteString.Char8 (pack, unpack)
import qualified Data.Map.Lazy         as Map
import           Data.Maybe
import           Data.Scientific
import qualified Data.Text             as Text
import           Data.Time.Clock.POSIX
import           Data.Typeable
import           Database.Persist
import           Database.Persist.Sql
import           GHC.Generics
import           GHC.TypeLits          (Symbol)
import           Models
import           Servant
import           Web.JWT               hiding (JSON)

data LoginRequest = LoginRequest {
    email    :: String,
    password :: String
} deriving Generic

instance FromJSON LoginRequest

data AuthResponse = AuthResponse {
  token :: Text.Text
} deriving Generic

instance ToJSON AuthResponse

validateLogin :: LoginRequest -> App Bool
validateLogin req = do
    usr <- runDb $ selectFirst [ Email ==. email (req :: LoginRequest)] []
    case usr of
      Nothing -> return False
      Just (Entity key user) -> do
        let pass = pack $ (password (req :: LoginRequest))
        return $ validatePassword (pack $ password ( user :: User)) pass

generateToken :: Entity User -> App Text.Text
generateToken (Entity key user) = do
    time <- liftIO getExpirationTime
    sec <- asks getSecret
    let cs = def {
        Web.JWT.exp = Just time
        , unregisteredClaims = Map.fromList [("id", (Number $ scientific (fromIntegral $ fromSqlKey key) 0))]
      }
    return $ encodeSigned HS256 sec cs

getExpirationTime :: IO NumericDate
getExpirationTime = do
    pTime <- getPOSIXTime -- POSIX is time in seconds since EPOCH
    return $ fromJust $ numericDate (pTime + 3600) -- hour

type AuthAPI =
    "login"
        :> ReqBody '[JSON] LoginRequest
        :> Post '[JSON] AuthResponse

authServer :: ServerT AuthAPI App
authServer =
             login
    where

    login :: LoginRequest -> App AuthResponse
    login req = do
        validLogin <- validateLogin req
        if validLogin then do
          usr <- runDb $ selectFirst [ Email ==. email (req :: LoginRequest)] []
          case usr of
            Nothing -> throwError err500
            Just usr -> do
              token <- generateToken usr
              return $ AuthResponse { token = token }
        else
          throwError err401 { errBody = "Invalid Login" }

