{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , DeriveGeneric
  #-}

module LocalCooking.Common.Tag.Diet where

import LocalCooking.Common.Tag (Tag)

import Control.Newtype (Newtype (..))
import Test.QuickCheck (Arbitrary)
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON, ToJSON)
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import GHC.Generics (Generic)



newtype DietTag = DietTag
  { getDietTag :: Tag
  } deriving (Eq, Ord, Show, PersistField, PersistFieldSql, Hashable, Generic, Arbitrary, FromJSON, ToJSON)

instance Newtype DietTag Tag where
  pack = DietTag
  unpack = getDietTag
