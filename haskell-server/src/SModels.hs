{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SModels (SterilizedUser, sterilizeUser) where

import           Data.Aeson
import           Database.Persist
import           GHC.Generics
import           Models

data SterilizedUser = SterilizedUser {
    id         :: Key User,
    email      :: String,
    firstname  :: String,
    lastname   :: String,
    teamnumber :: Int
} deriving Generic

instance ToJSON SterilizedUser
instance FromJSON SterilizedUser

sterilizeUser :: Entity User -> SterilizedUser
sterilizeUser (Entity uId u)  =
    SterilizedUser {
        id = uId,
        email = email (u :: User),
        firstname = firstname (u :: User),
        lastname = lastname (u :: User),
        teamnumber = teamnumber (u :: User)
    }
