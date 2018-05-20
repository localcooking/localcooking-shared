{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module Data.List.Pagination where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String, Object), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.Attoparsec (attoAeson)
import Data.Attoparsec.Text (Parser, string)
import Data.Hashable (Hashable)
import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)


data PaginationArgs = PaginationArgs
  { paginationArgsIndex    :: Int
  , paginationArgsSize     :: Int
  } deriving (Eq, Ord, Show, Generic)

instance Arbitrary PaginationArgs where
  arbitrary = PaginationArgs <$> arbitrary <*> arbitrary

instance ToJSON PaginationArgs where
  toJSON PaginationArgs{..} = object
    [ "index" .= paginationArgsIndex
    , "size" .= paginationArgsSize
    ]

instance FromJSON PaginationArgs where
  parseJSON json = case json of
    Object o -> PaginationArgs <$> o .: "index"
                               <*> o .: "size"
    _ -> typeMismatch "PaginationArgs" json
