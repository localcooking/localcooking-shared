{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module LocalCooking.Common.Tag where

import Data.Text (Text)
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (String))
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances ()


newtype Tag = Tag
  { printTag :: Text
  } deriving (Eq, Ord, Show, PersistField, PersistFieldSql, Hashable, Generic, ToJSON, FromJSON)

instance Arbitrary Tag where
  arbitrary = Tag <$> arbitrary
