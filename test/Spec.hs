{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}

import LocalCooking.Common.AccessToken (AccessToken)
import LocalCooking.Common.Diet (Diet)
import LocalCooking.Common.Ingredient (Ingredient)
import LocalCooking.Common.Rating (Rating)
import LocalCooking.Common.Tag (Tag)
import LocalCooking.Common.Order (OrderProgress)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.User.Role (UserRole)
import LocalCooking.Common.User.Password (HashedPassword)

import LocalCooking.Semantic.Mitch (ReviewSynopsis, Review, MealSynopsis, Meal, ChefSynopsis, Chef, MenuSynopsis, Menu)
import qualified LocalCooking.Semantic.Mitch as Mitch
import LocalCooking.Semantic.Chef (MealSettings, ChefSettings, MenuSettings)
import qualified LocalCooking.Semantic.Chef as Chef

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
  , testProperty "LocalCooking.Common.Rating" (\(x :: Rating) -> jsonIso x)
  , testProperty "LocalCooking.Common.Tag" (\(x :: Tag) -> jsonIso x)
  , testProperty "LocalCooking.Common.Order" (\(x :: OrderProgress) -> jsonIso x)
  , testProperty "LocalCooking.Common.User.Name" (\(x :: Name) -> jsonIso x)
  , testProperty "LocalCooking.Common.User.Role" (\(x :: UserRole) -> jsonIso x)
  , testProperty "LocalCooking.Common.User.Password" (\(x :: HashedPassword) -> jsonIso x)

  , testProperty "LocalCooking.Semantic.Mitch.ReviewSynopsis"
    (\(x :: ReviewSynopsis) -> jsonIso x)
  , testProperty "LocalCooking.Semantic.Mitch.Review"
    (\(x :: Review) -> jsonIso x)
  , testProperty "LocalCooking.Semantic.Mitch.MealSynopsis"
    (\(x :: MealSynopsis) -> jsonIso x)
  , testProperty "LocalCooking.Semantic.Mitch.Meal"
    (\(x :: Meal) -> jsonIso x)
  , testProperty "LocalCooking.Semantic.Mitch.ChefSynopsis"
    (\(x :: ChefSynopsis) -> jsonIso x)
  , testProperty "LocalCooking.Semantic.Mitch.Chef"
    (\(x :: Chef) -> jsonIso x)
  , testProperty "LocalCooking.Semantic.Mitch.MenuSynopsis"
    (\(x :: MenuSynopsis) -> jsonIso x)
  , testProperty "LocalCooking.Semantic.Mitch.Menu"
    (\(x :: Menu) -> jsonIso x)
  , testProperty "LocalCooking.Semantic.Mitch.Order"
    (\(x :: Mitch.Order) -> jsonIso x)

  , testProperty "LocalCooking.Semantic.Chef.MealSettings"
    (\(x :: MealSettings) -> jsonIso x)
  , testProperty "LocalCooking.Semantic.Chef.ChefSettings"
    (\(x :: ChefSettings) -> jsonIso x)
  , testProperty "LocalCooking.Semantic.Chef.MenuSettings"
    (\(x :: MenuSettings) -> jsonIso x)
  , testProperty "LocalCooking.Semantic.Chef.Order"
    (\(x :: Chef.Order) -> jsonIso x)
  ]


jsonIso :: FromJSON a => ToJSON a => Eq a => a -> Bool
jsonIso x = case Aeson.decode (Aeson.encode x) of
  Nothing -> False
  Just y -> x == y
