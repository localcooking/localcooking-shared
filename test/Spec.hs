{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}

import LocalCooking.Common.AuthToken (AuthToken)
import LocalCooking.Common.Password (HashedPassword)

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson


main :: IO ()
main = defaultMain $ testGroup "JSON encodings"
  [ testProperty "AuthToken" (\(x :: AuthToken) -> jsonIso x)
  , testProperty "HashedPassword" (\(x :: HashedPassword) -> jsonIso x)
  ]


jsonIso :: FromJSON a => ToJSON a => Eq a => a -> Bool
jsonIso x = case Aeson.decode (Aeson.encode x) of
  Nothing -> False
  Just y -> x == y
