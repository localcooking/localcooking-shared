{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module Data.Text.Markdown where

import Data.Text.Lazy (Text)
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (String))
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances ()


newtype MarkdownText = MarkdownText
  { getMarkdownText :: Text
  } deriving (Eq, Ord, Show, PersistField, PersistFieldSql, Hashable, Generic, ToJSON, FromJSON)

instance Arbitrary MarkdownText where
  arbitrary = MarkdownText <$> arbitrary
