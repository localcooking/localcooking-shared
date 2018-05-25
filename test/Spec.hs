{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}

import LocalCooking.Common.AccessToken (AccessToken)
import LocalCooking.Common.Diet (Diet)
import LocalCooking.Common.Ingredient (Ingredient)
import LocalCooking.Common.Order (OrderProgress)
import LocalCooking.Common.Rating (Rating)
import LocalCooking.Common.Tag (Tag)
import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.User.Role (UserRole)
import LocalCooking.Common.User.Password (HashedPassword)

import Data.Address (USAAddress)
import Data.Image.Source (ImageSource)
import Data.List.Pagination (PaginationArgs)
import Data.List.Sorting (SortingArgs)
import Data.Price (Price)
import Data.Text.Markdown (MarkdownText)
import Data.Text.Permalink (Permalink)


import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson


main :: IO ()
main = defaultMain $ testGroup "JSON encodings"
  [ testProperty "Data.Address" (\(x :: USAAddress) -> jsonIso x)
  , testProperty "Data.Image.Source" (\(x :: ImageSource) -> jsonIso x)
  , testProperty "Data.List.Pagination" (\(x :: PaginationArgs) -> jsonIso x)
  , testProperty "Data.List.Sorting" (\(x :: SortingArgs ()) -> jsonIso x)
  , testProperty "Data.Price" (\(x :: Price) -> jsonIso x)
  , testProperty "Data.Text.Markdown" (\(x :: MarkdownText) -> jsonIso x)
  , testProperty "Data.Text.Permalink" (\(x :: Permalink) -> jsonIso x)
  , testProperty "LocalCooking.Common.AccessToken" (\(x :: AccessToken) -> jsonIso x)
  , testProperty "LocalCooking.Common.Diet" (\(x :: Diet) -> jsonIso x)
  , testProperty "LocalCooking.Common.Ingredient" (\(x :: Ingredient) -> jsonIso x)
  , testProperty "LocalCooking.Common.Order" (\(x :: OrderProgress) -> jsonIso x)
  , testProperty "LocalCooking.Common.Rating" (\(x :: Rating) -> jsonIso x)
  , testProperty "LocalCooking.Common.Tag" (\(x :: Tag) -> jsonIso x)
  , testProperty "LocalCooking.Common.User.Name" (\(x :: Name) -> jsonIso x)
  , testProperty "LocalCooking.Common.User.Role" (\(x :: UserRole) -> jsonIso x)
  , testProperty "LocalCooking.Common.User.Password" (\(x :: HashedPassword) -> jsonIso x)

  ]


jsonIso :: FromJSON a => ToJSON a => Eq a => a -> Bool
jsonIso x = case Aeson.decode (Aeson.encode x) of
  Nothing -> False
  Just y -> x == y
