
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Auth where

import           Config
import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Reader
import           Data.Aeson.Types                 (Value (..))
import           Data.ByteString
import           Data.Int
import           Data.Map
import           Data.Maybe
import           Data.Scientific
import qualified Data.Text                        as Text
import           Data.Text.Encoding
import           Data.Time.Clock.POSIX            hiding (getCurrentTime)
import           Data.Typeable
import           Database.Persist.Sql
import           GHC.Generics
import           GHC.TypeLits                     (Symbol)
import           Models
import           Network.Wai
import           Servant                          hiding (Unauthorized)
import           Servant.Server.Experimental.Auth
import           Web.JWT

type instance AuthServerData (AuthProtect "jwt") = Entity User

authHandler :: Config -> AuthHandler Request (Entity User)
authHandler cfg =
  let handler req =
        case Prelude.lookup "Authorization" (requestHeaders req) of
          Nothing -> throwError (err401 { errBody = "Missing auth header" }) -- TODO: make this a const to return JSON
          Just authToken -> do
            parsedToken <- liftIO $ parseToken authToken cfg
            case parsedToken of
              InvalidToken -> throwError (err401 { errBody = "InvalidToken" })
              ExpiredToken -> throwError (err401 { errBody = "ExpiredToken" })
              Success key -> do
                usr <- liftIO $ runSqlPool (getEntity key) (getPool cfg)
                case usr of
                  Nothing -> throwError (err401 { errBody = "Invalid User" })
                  Just u  -> return u
  in mkAuthHandler handler

data ParseOutput =
  InvalidToken |
  ExpiredToken |
  Success (Key User)

parseToken :: ByteString -> Config -> IO ParseOutput
parseToken s cfg = do
  let secret = Config.getSecret cfg
  case decodeAndVerifySignature secret str of
    Nothing -> return InvalidToken
    Just mJwt -> do
      let c = claims mJwt
      case Web.JWT.exp c of
        Nothing -> return InvalidToken
        Just e -> do
          time <- getCurrentTime
          if e < time then
            return ExpiredToken
          else do
            let idMay = Data.Map.lookup ("id" :: Text.Text) (unregisteredClaims c)
            case idMay of
              Nothing -> return InvalidToken
              Just idNum ->
                case idNum of
                  Number i ->
                    case toBoundedInteger i of
                      Nothing  -> return InvalidToken
                      Just int -> return $ Success $ toSqlKey int
                  _ -> return InvalidToken
  where
    str = decodeUtf8 s

getCurrentTime :: IO NumericDate
getCurrentTime = do
    pTime <- getPOSIXTime
    return $ fromJust $ numericDate pTime

genAuthServerContext :: Config -> Context (AuthHandler Request (Entity User) ': '[])
genAuthServerContext cfg = (authHandler cfg) :. EmptyContext

