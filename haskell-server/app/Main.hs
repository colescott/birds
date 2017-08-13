{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import           Api                         (app)
import           Auth
import           Config
import           Control.Monad.Except        (ExceptT, MonadError)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Data.ByteString.Char8       (ByteString, pack)
import           Data.Maybe
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Models
import           Network.Wai
import           Network.Wai.Handler.Warp    (run)
import           Safe                        (readMay)
import           Servant
import           System.Environment          (lookupEnv)
import           Text.Read

main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8081
    pool <- makePool env
    sec <- makeSecret env
    let cfg = Config { getPool = pool, getSecret = sec, getEnv = env }
        logger = setLogger env
    runSqlPool (runMigration migrateAll) pool

    putStrLn $ "Starting server on port " ++ show port ++ "."
    run port $ logger $ app cfg

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing ->
            return def
        Just str ->
            maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
        error $ mconcat
            [ "Failed to read [["
            , str
            , "]] for environment variable "
            , env
            ]

