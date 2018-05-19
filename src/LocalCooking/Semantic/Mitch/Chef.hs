{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  #-}

module LocalCooking.Semantic.Mitch.Chef where

import LocalCooking.Semantic.Mitch.Menu (MenuSynopsis)
import LocalCooking.Semantic.Mitch.Review (ReviewSynopsis)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Rating (Rating)

import Data.Image.Source (ImageSource)
import Data.Text (Text)
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), object, Value (Object), (.=))
import Data.Aeson.Types (typeMismatch)
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances ()


data ChefSynopsis = ChefSynopsis
  { chefSynopsisName   :: Name
  , chefSynopsisImage  :: ImageSource
  , chefSynopsisRating :: Rating
  , chefSynopsisOrders :: Int
  , chefSynopsisTags   :: [ChefTag]
  } deriving (Eq, Show, Generic)


instance Arbitrary ChefSynopsis where
  arbitrary = ChefSynopsis <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

instance ToJSON ChefSynopsis where
  toJSON ChefSynopsis{..} = object
    [ "name" .= chefSynopsisName
    , "image" .= chefSynopsisImage
    , "rating" .= chefSynopsisRating
    , "orders" .= chefSynopsisOrders
    , "tags" .= chefSynopsisTags
    ]

instance FromJSON ChefSynopsis where
  parseJSON json = case json of
    Object o -> ChefSynopsis <$> o .: "name"
                             <*> o .: "image"
                             <*> o .: "rating"
                             <*> o .: "orders"
                             <*> o .: "tags"
    _ -> typeMismatch "ChefSynopsis" json


data Chef = Chef
  { chefName    :: Name
  , chefImage   :: ImageSource
  , chefRating  :: Rating
  , chefReviews :: [ReviewSynopsis]
  , chefOrders  :: Int
  , chefTags    :: [ChefTag]
  , chefMenus   :: [MenuSynopsis]
  } deriving (Eq, Show, Generic)


instance Arbitrary Chef where
  arbitrary = Chef <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary

instance ToJSON Chef where
  toJSON Chef{..} = object
    [ "name" .= chefName
    , "image" .= chefImage
    , "rating" .= chefRating
    , "orders" .= chefOrders
    , "tags" .= chefTags
    ]

instance FromJSON Chef where
  parseJSON json = case json of
    Object o -> Chef <$> o .: "name"
                             <*> o .: "image"
                             <*> o .: "rating"
                             <*> o .: "orders"
                             <*> o .: "tags"
    _ -> typeMismatch "Chef" json
