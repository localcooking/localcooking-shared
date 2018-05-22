{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  #-}

module Data.List.Pagination where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))


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
