{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module LocalCooking.Common.Pagination where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String, Object), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.Attoparsec (attoAeson)
import Data.Attoparsec.Text (Parser, string)
import Data.Hashable (Hashable)
import Control.Applicative ((<|>))
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)


data SortOrdering = Ascending | Descending
  deriving (Eq, Ord, Enum, Show, Read, Generic)

instance Arbitrary SortOrdering where
  arbitrary = oneof [pure Ascending, pure Descending]

instance Hashable SortOrdering

instance ToJSON SortOrdering where
  toJSON x = String $ case x of
    Ascending -> "asc"
    Descending -> "des"

instance FromJSON SortOrdering where
  parseJSON = attoAeson sortOrderingParser

sortOrderingParser :: Parser SortOrdering
sortOrderingParser = do
  let asc = Ascending <$ string "asc"
      des = Descending <$ string "des"
  asc <|> des


-- | Pagination datum for any type of json-encodable field-accessor representation datum
data PaginationArgs field = PaginationArgs
  { paginationArgsOrdering :: SortOrdering
  , paginationArgsIndex    :: Int
  , paginationArgsSize     :: Int
  , paginationArgsField    :: field
  } deriving (Eq, Ord, Show, Generic)

instance Arbitrary field => Arbitrary (PaginationArgs field) where
  arbitrary = PaginationArgs <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance ToJSON field => ToJSON (PaginationArgs field) where
  toJSON PaginationArgs{..} = object
    [ "ordering" .= paginationArgsOrdering
    , "index" .= paginationArgsIndex
    , "size" .= paginationArgsSize
    , "field" .= paginationArgsField
    ]

instance FromJSON field => FromJSON (PaginationArgs field) where
  parseJSON json = case json of
    Object o -> PaginationArgs <$> o .: "ordering"
                               <*> o .: "index"
                               <*> o .: "size"
                               <*> o .: "field"
    _ -> typeMismatch "PaginationArgs" json
