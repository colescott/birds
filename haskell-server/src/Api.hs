{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Api (app) where

import           Api.Auth
import           Api.Users
import           Auth
import           Config
import           Control.Monad.Except   (ExceptT, MonadError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader   (ReaderT, runReaderT)
import           Models
import           Servant

type CombinedAPI = "users" :> UserAPI
              :<|> "auth" :> AuthAPI

combinedServer :: ServerT CombinedAPI App
combinedServer = userServer
                 :<|> authServer

combinedAPI :: Proxy CombinedAPI
combinedAPI = Proxy

convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

appToServer :: Config -> Server CombinedAPI
appToServer cfg = enter (convertApp cfg) combinedServer

app :: Config -> Application
app cfg = serveWithContext combinedAPI (genAuthServerContext cfg) $ appToServer cfg

