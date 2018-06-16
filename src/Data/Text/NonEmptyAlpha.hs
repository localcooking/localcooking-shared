{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module Data.Text.NonEmptyAlpha where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON, ToJSON)
import Data.List.NonEmpty (NonEmpty)
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (listOf1, elements)
import Test.QuickCheck.Instances ()


newtype NonEmptyAlphaText = NonEmptyAlphaText
  { getNonEmptyAlphaText :: Text
  } deriving (Eq, Ord, PersistField, PersistFieldSql, Hashable, Generic, ToJSON, FromJSON)

instance Show NonEmptyAlphaText where
  show (NonEmptyAlphaText t) = T.unpack t

instance Arbitrary NonEmptyAlphaText where
  arbitrary = (NonEmptyAlphaText . T.pack) <$> listOf1 (elements ['a' .. 'z'])
