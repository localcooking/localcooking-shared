{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  #-}

module Data.List.Sorting where

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
data SortingArgs field = SortingArgs
  { sortingArgsOrdering :: SortOrdering
  , sortingArgsField    :: field
  } deriving (Eq, Ord, Show, Generic)

instance Arbitrary field => Arbitrary (SortingArgs field) where
  arbitrary = SortingArgs <$> arbitrary <*> arbitrary

instance ToJSON field => ToJSON (SortingArgs field) where
  toJSON SortingArgs{..} = object
    [ "ordering" .= sortingArgsOrdering
    , "field" .= sortingArgsField
    ]

instance FromJSON field => FromJSON (SortingArgs field) where
  parseJSON json = case json of
    Object o -> SortingArgs <$> o .: "ordering"
                            <*> o .: "field"
    _ -> typeMismatch "SortingArgs" json
