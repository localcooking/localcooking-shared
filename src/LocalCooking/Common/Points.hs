{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  , DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Common.Points where

import Data.Aeson (FromJSON, ToJSON)
import Data.Hashable (Hashable)
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary)


newtype Points = Points
  { getPoints :: Int
  } deriving (Eq, Ord, Num, Enum, Show, Read, Generic, Hashable, PersistField, PersistFieldSql, FromJSON, ToJSON, Arbitrary)
