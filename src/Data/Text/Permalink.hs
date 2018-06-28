{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module Data.Text.Permalink where

import Data.Text (Text)
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON, ToJSON)
import Data.Attoparsec.Text (Parser, takeWhile1, inClass)
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances ()


newtype Permalink = Permalink
  { printPermalink :: Text
  } deriving (Eq, Ord, Show, PersistField, PersistFieldSql, Hashable, Generic, ToJSON, FromJSON)

instance Arbitrary Permalink where
  arbitrary = Permalink <$> arbitrary


-- | Suitable for use within nested-routes or other parsing schemes
permalinkParser :: Parser Permalink
permalinkParser = Permalink <$> takeWhile1 (inClass $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_")
