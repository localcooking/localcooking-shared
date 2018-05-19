{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  #-}

module LocalCooking.Semantic.Mitch.Menu where

import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Semantic.Mitch.Meal (MealSynopsis)
import LocalCooking.Semantic.Mitch.Chef (ChefSynopsis)

import Data.Time.Calendar (Day)
import Data.Text.Markdown (MarkdownText)
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


data MenuSynopsis = MenuSynopsis
  { menuSynopsisPublished :: Day
  , menuSynopsisDeadline  :: Day
  , menuSynopsisHeadline  :: Text
  , menuSynopsisTags      :: [MealTag]
  } deriving (Eq, Show, Generic)


instance Arbitrary MenuSynopsis where
  arbitrary = MenuSynopsis <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

instance ToJSON MenuSynopsis where
  toJSON MenuSynopsis{..} = object
    [ "published" .= menuSynopsisPublished
    , "deadline" .= menuSynopsisDeadline
    , "headline" .= menuSynopsisHeadline
    , "tags" .= menuSynopsisTags
    ]

instance FromJSON MenuSynopsis where
  parseJSON json = case json of
    Object o -> MenuSynopsis <$> o .: "published"
                             <*> o .: "deadline"
                             <*> o .: "headline"
                             <*> o .: "tags"
    _ -> typeMismatch "MenuSynopsis" json


data Menu = Menu
  { menuPublished   :: Day
  , menuDeadline    :: Day
  , menuDescription :: MarkdownText
  , menuAuthor      :: ChefSynopsis
  , menuMeals       :: [MealSynopsis]
  } deriving (Eq, Show, Generic)


instance Arbitrary Menu where
  arbitrary = Menu <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary

instance ToJSON Menu where
  toJSON Menu{..} = object
    [ "published" .= menuPublished
    , "deadline" .= menuDeadline
    , "description" .= menuDescription
    , "author" .= menuAuthor
    , "meals" .= menuMeals
    ]

instance FromJSON Menu where
  parseJSON json = case json of
    Object o -> Menu <$> o .: "published"
                     <*> o .: "deadline"
                     <*> o .: "description"
                     <*> o .: "author"
                     <*> o .: "meals"
    _ -> typeMismatch "Menu" json
