{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module Data.Image.Source where

import Data.Hashable (Hashable)
import Data.Aeson (FromJSON, ToJSON)
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances ()


newtype ImageSource = ImageSource
  { getImageSource :: Int
  } deriving (Eq, Ord, Enum, Num, PersistField, PersistFieldSql, Hashable, Generic, ToJSON, FromJSON)

instance Show ImageSource where
  show (ImageSource x) = show x

instance Arbitrary ImageSource where
  arbitrary = ImageSource <$> arbitrary
