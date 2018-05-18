{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  #-}

module LocalCooking.Common.Meal where

import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.Ingredient (Ingredient)
import LocalCooking.Common.Diet (Diet)

import Data.Image.Source (ImageSource)
import Data.Text (Text)
import Data.Text.Permalink (Permalink)
import Data.Text.Markdown (MarkdownText)
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (Object), (.=), object, (.:))
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances ()


data MealSynopsis = MealSynopsis
  { mealSynopsisTitle     :: Text
  , mealSynopsisPermalink :: Permalink
  , mealSynopsisHeading   :: Text
  , mealSynopsisImages    :: [ImageSource]
  , mealSynopsisRating    :: Rating
  , mealSynopsisOrders    :: Int
  , mealSynopsisTags      :: [MealTag]
  , mealSynopsisDiets     :: [Diet]
  }


data Meal = Meal
  { mealTitle        :: Text
  , mealPermalink    :: Permalink
  , mealDescription  :: MarkdownText
  , mealInstructions :: MarkdownText
  , mealImages       :: [ImageSource]
  , mealIngredients  :: [Ingredient]
  , mealDiets        :: [Diet]
  , mealTags         :: [MealTag]
  , mealOrders       :: Int
  , mealRating       :: Rating
  , mealReviews      :: [ReviewSynopsis]
  } deriving (Eq, Show, Generic)

instance ToJSON Meal where
  toJSON Meal{..} = object
    [ "title" .= mealTitle
    , "synopsis" .= mealSynopsis
    , "description" .= mealDescription
    , "instructions" .= mealInstructions
    , "images" .= mealImages
    , "ingredients" .= mealIngredients
    , "tags" .= mealTags
    ]

instance FromJSON Meal where
  parseJSON json = case json of
    Object o -> Meal <$> o .: "title"
                     <*> o .: "synopsis"
                     <*> o .: "description"
                     <*> o .: "instructions"
                     <*> o .: "images"
                     <*> o .: "ingredients"
                     <*> o .: "tags"
    _ -> typeMismatch "Meal" json
