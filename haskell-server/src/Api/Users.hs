{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Api.Users (UserAPI, userServer) where

import           Auth
import           Config
import           Control.Monad.Reader
import           Crypto.BCrypt
import           Data.Aeson.Types
import           Data.ByteString.Char8         (pack, unpack)
import           Data.Maybe
import           Database.Persist
import           Database.Persist.Sql
import           GHC.Generics
import           Models
import           Servant
import           Servant.API.Experimental.Auth (AuthProtect)
import           SModels

type UserAPI =
        AuthProtect "jwt"
        :> Get '[JSON] [SterilizedUser]
    :<|>
        Capture "id" Int
        :> Get '[JSON] (Maybe SterilizedUser)
    :<|>
        ReqBody '[JSON] User
        :> Post '[JSON] (Maybe SterilizedUser)

userServer :: ServerT UserAPI App
userServer =
             getUsers
        :<|> getUserById
        :<|> createUser
    where

    getUsers :: Entity User -> App [SterilizedUser]
    getUsers auth = do
        userList <- runDb $ selectList [] [ Desc persistIdField ]
        return $ map sterilizeUser userList

    getUserById :: Int -> App (Maybe SterilizedUser)
    getUserById qId = do
            usr <- runDb $ getEntity $ toSqlKey $ fromIntegral qId
            return $ case usr of
                Nothing -> Nothing
                Just u  -> Just $ sterilizeUser u

    createUser :: User -> App (Maybe SterilizedUser)
    createUser userReq = do
        newPass <- liftIO $ hashPasswordUsingPolicy hashingPolicy $ pack $ password userReq
        case newPass of
          Nothing -> return Nothing
          Just pass -> do
            let user = userReq {password = unpack pass}
            uId <- runDb $ insert user
            usr <- runDb $ getEntity uId
            return $ case usr of
                       Nothing -> Nothing
                       Just u  -> Just $ sterilizeUser u
