{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module LocalCooking.Semantic.Mitch where

import LocalCooking.Common.Rating (Rating)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Tag.Meal (MealTag)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.Diet (Diet)
import LocalCooking.Common.Ingredient (Ingredient)

import Data.Price (Price)
import Data.Image.Source (ImageSource)
import Data.Text (Text)
import Data.Text.Permalink (Permalink)
import Data.Text.Markdown (MarkdownText)
import Data.Time.Calendar (Day)
import Data.Hashable (Hashable)
import Data.Aeson (FromJSON (..), ToJSON (toJSON), Value (Object), (.=), object, (.:))
import Data.Aeson.Types (typeMismatch)
import GHC.Generics (Generic)
import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances ()


-- * Reviews

-- | Global enumerator identifier for review permalinks - store in database globally
-- as nonce counter
newtype ReviewId = ReviewId
  { getReviewId :: Int
  } deriving (Eq, Ord, Show, PersistField, PersistFieldSql, Hashable, Generic, ToJSON, FromJSON)

instance Arbitrary ReviewId where
  arbitrary = ReviewId <$> arbitrary


data ReviewSynopsis = ReviewSynopsis
  { reviewSynopsisRating  :: Rating
  , reviewSynopsisHeading :: Text
  , reviewSynopsisId      :: ReviewId
  } deriving (Eq, Show, Generic)

instance Arbitrary ReviewSynopsis where
  arbitrary = ReviewSynopsis <$> arbitrary
                             <*> arbitrary
                             <*> arbitrary

instance ToJSON ReviewSynopsis where
  toJSON ReviewSynopsis{..} = object
    [ "rating" .= reviewSynopsisRating
    , "heading" .= reviewSynopsisHeading
    , "id" .= reviewSynopsisId
    ]

instance FromJSON ReviewSynopsis where
  parseJSON json = case json of
    Object o -> ReviewSynopsis <$> o .: "rating"
                               <*> o .: "heading"
                               <*> o .: "id"
    _ -> typeMismatch "ReviewSynopsis" json


data Review = Review
  { reviewRating  :: Rating
  , reviewHeading :: Text
  , reviewId      :: ReviewId
  , reviewBody    :: MarkdownText
  , reviewImages  :: [ImageSource]
  } deriving (Eq, Show, Generic)

instance Arbitrary Review where
  arbitrary = Review <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary


instance ToJSON Review where
  toJSON Review{..} = object
    [ "rating" .= reviewRating
    , "heading" .= reviewHeading
    , "id" .= reviewId
    , "body" .= reviewBody
    , "images" .= reviewImages
    ]

instance FromJSON Review where
  parseJSON json = case json of
    Object o -> Review <$> o .: "rating"
                       <*> o .: "heading"
                       <*> o .: "id"
                       <*> o .: "body"
                       <*> o .: "images"
    _ -> typeMismatch "Review" json


-- * Menus

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


-- * Meals

data MealSynopsis = MealSynopsis
  { mealSynopsisTitle     :: Text
  , mealSynopsisPermalink :: Permalink
  , mealSynopsisHeading   :: Text
  , mealSynopsisImages    :: [ImageSource]
  , mealSynopsisRating    :: Rating
  , mealSynopsisOrders    :: Int
  , mealSynopsisTags      :: [MealTag]
  , mealSynopsisDiets     :: [Diet]
  , mealSynopsisPrice     :: Price
  } deriving (Eq, Show, Generic)


instance Arbitrary MealSynopsis where
  arbitrary = MealSynopsis <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary


instance ToJSON MealSynopsis where
  toJSON MealSynopsis{..} = object
    [ "title" .= mealSynopsisTitle
    , "permalink" .= mealSynopsisPermalink
    , "heading" .= mealSynopsisHeading
    , "images" .= mealSynopsisImages
    , "rating" .= mealSynopsisRating
    , "orders" .= mealSynopsisOrders
    , "tags" .= mealSynopsisTags
    , "diets" .= mealSynopsisDiets
    , "price" .= mealSynopsisPrice
    ]

instance FromJSON MealSynopsis where
  parseJSON json = case json of
    Object o -> MealSynopsis <$> o .: "title"
                             <*> o .: "permalink"
                             <*> o .: "heading"
                             <*> o .: "images"
                             <*> o .: "rating"
                             <*> o .: "orders"
                             <*> o .: "tags"
                             <*> o .: "diets"
                             <*> o .: "price"
    _ -> typeMismatch "MealSynopsis" json


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
  , mealPrice        :: Price
  } deriving (Eq, Show, Generic)


instance Arbitrary Meal where
  arbitrary = Meal <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary

instance ToJSON Meal where
  toJSON Meal{..} = object
    [ "title" .= mealTitle
    , "permalink" .= mealPermalink
    , "description" .= mealDescription
    , "instructions" .= mealInstructions
    , "images" .= mealImages
    , "ingredients" .= mealIngredients
    , "diets" .= mealDiets
    , "tags" .= mealTags
    , "orders" .= mealOrders
    , "rating" .= mealRating
    , "reviews" .= mealReviews
    , "price" .= mealPrice
    ]

instance FromJSON Meal where
  parseJSON json = case json of
    Object o -> Meal <$> o .: "title"
                     <*> o .: "permalink"
                     <*> o .: "description"
                     <*> o .: "instructions"
                     <*> o .: "images"
                     <*> o .: "ingredients"
                     <*> o .: "diets"
                     <*> o .: "tags"
                     <*> o .: "orders"
                     <*> o .: "rating"
                     <*> o .: "reviews"
                     <*> o .: "price"
    _ -> typeMismatch "Meal" json


-- * Chefs

data ChefSynopsis = ChefSynopsis
  { chefSynopsisName      :: Name
  , chefSynopsisPermalink :: Permalink
  , chefSynopsisImage     :: ImageSource
  , chefSynopsisRating    :: Rating
  , chefSynopsisOrders    :: Int
  , chefSynopsisTags      :: [ChefTag]
  } deriving (Eq, Show, Generic)


instance Arbitrary ChefSynopsis where
  arbitrary = ChefSynopsis <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

instance ToJSON ChefSynopsis where
  toJSON ChefSynopsis{..} = object
    [ "name" .= chefSynopsisName
    , "permalink" .= chefSynopsisPermalink
    , "image" .= chefSynopsisImage
    , "rating" .= chefSynopsisRating
    , "orders" .= chefSynopsisOrders
    , "tags" .= chefSynopsisTags
    ]

instance FromJSON ChefSynopsis where
  parseJSON json = case json of
    Object o -> ChefSynopsis <$> o .: "name"
                             <*> o .: "permalink"
                             <*> o .: "image"
                             <*> o .: "rating"
                             <*> o .: "orders"
                             <*> o .: "tags"
    _ -> typeMismatch "ChefSynopsis" json


data Chef = Chef
  { chefName      :: Name
  , chefPermalink :: Permalink
  , chefImage     :: ImageSource
  , chefRating    :: Rating
  , chefReviews   :: [ReviewSynopsis]
  , chefOrders    :: Int
  , chefTags      :: [ChefTag]
  , chefMenus     :: [MenuSynopsis]
  } deriving (Eq, Show, Generic)


instance Arbitrary Chef where
  arbitrary = Chef <$> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary
                   <*> arbitrary

instance ToJSON Chef where
  toJSON Chef{..} = object
    [ "name" .= chefName
    , "permalink" .= chefPermalink
    , "image" .= chefImage
    , "rating" .= chefRating
    , "reviews" .= chefReviews
    , "orders" .= chefOrders
    , "tags" .= chefTags
    , "menus" .= chefMenus
    ]

instance FromJSON Chef where
  parseJSON json = case json of
    Object o -> Chef <$> o .: "name"
                     <*> o .: "permalink"
                     <*> o .: "image"
                     <*> o .: "rating"
                     <*> o .: "reviews"
                     <*> o .: "orders"
                     <*> o .: "tags"
                     <*> o .: "menus"
    _ -> typeMismatch "Chef" json
