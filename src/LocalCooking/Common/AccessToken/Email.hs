{-# LANGUAGE
    DeriveGeneric
  , MultiParamTypeClasses
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Common.AccessToken.Email where

import LocalCooking.Common.AccessToken (AccessToken)

import Control.Newtype (Newtype (..))
import Test.QuickCheck (Arbitrary)
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON, ToJSON)
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import GHC.Generics (Generic)



newtype EmailToken = EmailToken
  { getEmailToken :: AccessToken
  } deriving (Eq, Show, PersistField, PersistFieldSql, Hashable, Generic, Arbitrary, FromJSON, ToJSON)

instance Newtype EmailToken AccessToken where
  pack = EmailToken
  unpack = getEmailToken
