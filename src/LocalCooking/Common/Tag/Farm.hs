{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , DeriveGeneric
  #-}

module LocalCooking.Common.Tag.Farm where

import LocalCooking.Common.Tag (Tag)

import Control.Newtype (Newtype (..))
import Test.QuickCheck (Arbitrary)
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON, ToJSON)
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import GHC.Generics (Generic)



newtype FarmTag = FarmTag
  { getFarmTag :: Tag
  } deriving (Eq, Ord, Show, PersistField, PersistFieldSql, Hashable, Generic, Arbitrary, FromJSON, ToJSON)

instance Newtype FarmTag Tag where
  pack = FarmTag
  unpack = getFarmTag
