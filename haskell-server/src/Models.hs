{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Models (EntityField(..), User(..), runDb, migrateAll, hashingPolicy) where

import           Config
import           Control.Monad.Reader
import           Crypto.BCrypt
import           Data.Aeson
import           Data.ByteString.Char8 (pack, unpack)
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.TH
import           GHC.Generics
import           Servant.Elm

share [mkPersist (sqlSettings {mpsPrefixFields = False}), mkMigrate "migrateAll"] [persistLowerCase|
User json
    email String
    firstname String
    lastname String
    teamnumber Int
    password String
    UniqueE email
    deriving Show Generic
|]

hashingPolicy :: HashingPolicy
hashingPolicy = HashingPolicy 8 $ pack "$2b$"

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

instance ElmType User

