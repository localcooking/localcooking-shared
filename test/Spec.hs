{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}

import LocalCooking.Common.AccessToken (AccessToken)
import LocalCooking.Common.Diet (Diet)
import LocalCooking.Common.Ingredient (Ingredient)
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Common.Rating (Rating)
import LocalCooking.Common.Tag (Tag)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.User.Role (UserRole)

import LocalCooking.Semantic.Mitch.Review (ReviewSynopsis, Review)
import LocalCooking.Semantic.Mitch.Meal (MealSynopsis, Meal)
import LocalCooking.Semantic.Mitch.Chef (ChefSynopsis)
import LocalCooking.Semantic.Mitch.Menu (Menu)

import Data.Text.Markdown (MarkdownText)
import Data.Text.Permalink (Permalink)
import Data.Image.Source (ImageSource)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson


main :: IO ()
main = defaultMain $ testGroup "JSON encodings"
  [ testProperty "Data.Text.Markdown" (\(x :: MarkdownText) -> jsonIso x)
  , testProperty "Data.Text.Permalink" (\(x :: Permalink) -> jsonIso x)
  , testProperty "Data.Image.Source" (\(x :: ImageSource) -> jsonIso x)
  , testProperty "LocalCooking.Common.AccessToken" (\(x :: AccessToken) -> jsonIso x)
  , testProperty "LocalCooking.Common.Diet" (\(x :: Diet) -> jsonIso x)
  , testProperty "LocalCooking.Common.Ingredient" (\(x :: Ingredient) -> jsonIso x)
  , testProperty "LocalCooking.Common.Password" (\(x :: HashedPassword) -> jsonIso x)
  , testProperty "LocalCooking.Common.Rating" (\(x :: Rating) -> jsonIso x)
  , testProperty "LocalCooking.Common.Tag" (\(x :: Tag) -> jsonIso x)
  , testProperty "LocalCooking.Common.User.Name" (\(x :: Name) -> jsonIso x)
  , testProperty "LocalCooking.Common.User.Role" (\(x :: UserRole) -> jsonIso x)

  , testProperty "LocalCooking.Semantic.Mitch.Review.ReviewSynopsis"
    (\(x :: ReviewSynopsis) -> jsonIso x)
  , testProperty "LocalCooking.Semantic.Mitch.Review.Review"
    (\(x :: Review) -> jsonIso x)
  , testProperty "LocalCooking.Semantic.Mitch.Meal.MealSynopsis"
    (\(x :: MealSynopsis) -> jsonIso x)
  , testProperty "LocalCooking.Semantic.Mitch.Meal.Meal"
    (\(x :: Meal) -> jsonIso x)
  , testProperty "LocalCooking.Semantic.Mitch.Chef.ChefSynopsis"
    (\(x :: ChefSynopsis) -> jsonIso x)
  -- , testProperty "LocalCooking.Semantic.Mitch.Menu.MenuSynopsis"
  --   (\(x :: MenuSynopsis) -> jsonIso x)
  , testProperty "LocalCooking.Semantic.Mitch.Menu.Menu"
    (\(x :: Menu) -> jsonIso x)
  ]


jsonIso :: FromJSON a => ToJSON a => Eq a => a -> Bool
jsonIso x = case Aeson.decode (Aeson.encode x) of
  Nothing -> False
  Just y -> x == y
