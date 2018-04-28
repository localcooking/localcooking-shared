{-# LANGUAGE
    DeriveGeneric
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Common.AccessToken.Permission where

import LocalCooking.Common.AccessToken (AccessToken)

import Control.Newtype (Newtype (..))
import Test.QuickCheck (Arbitrary)
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON, ToJSON)
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import GHC.Generics (Generic)



newtype PermissionToken = PermissionToken
  { getPermissionToken :: AccessToken
  } deriving (Eq, Show, PersistField, PersistFieldSql, Hashable, Generic, Arbitrary, FromJSON, ToJSON)

instance Newtype PermissionToken AccessToken where
  pack = PermissionToken
  unpack = getPermissionToken
