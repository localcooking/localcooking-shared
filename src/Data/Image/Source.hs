{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module Data.Image.Source where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String))
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (String))
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances ()


newtype ImageSource = ImageSource
  { getImageSource :: Int
  } deriving (Eq, Ord, Enum, Show, PersistField, PersistFieldSql, Hashable, Generic, ToJSON, FromJSON)

instance Arbitrary ImageSource where
  arbitrary = ImageSource <$> arbitrary
