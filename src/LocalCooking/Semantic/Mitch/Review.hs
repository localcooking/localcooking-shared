{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  #-}

module LocalCooking.Semantic.Mitch.Review where

import LocalCooking.Common.Rating (Rating)

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




data ReviewSynopsis = ReviewSynopsis
  { reviewSynopsisRating  :: Rating
  , reviewSynopsisHeading :: Text
  } deriving (Eq, Show, Generic)

instance Arbitrary ReviewSynopsis where
  arbitrary = ReviewSynopsis <$> arbitrary
                             <*> arbitrary

instance ToJSON ReviewSynopsis where
  toJSON ReviewSynopsis{..} = object
    [ "rating" .= reviewSynopsisRating
    , "heading" .= reviewSynopsisHeading
    ]

instance FromJSON ReviewSynopsis where
  parseJSON json = case json of
    Object o -> ReviewSynopsis <$> o .: "rating"
                               <*> o .: "heading"
    _ -> typeMismatch "ReviewSynopsis" json


data Review = Review
  { reviewRating  :: Rating
  , reviewHeading :: Text
  , reviewBody    :: MarkdownText
  , reviewImages  :: [ImageSource]
  } deriving (Eq, Show, Generic)

instance Arbitrary Review where
  arbitrary = Review <$> arbitrary
                     <*> arbitrary
                     <*> arbitrary
                     <*> arbitrary


instance ToJSON Review where
  toJSON Review{..} = object
    [ "rating" .= reviewRating
    , "heading" .= reviewHeading
    , "body" .= reviewBody
    , "images" .= reviewImages
    ]

instance FromJSON Review where
  parseJSON json = case json of
    Object o -> Review <$> o .: "rating"
                       <*> o .: "heading"
                       <*> o .: "body"
                       <*> o .: "images"
    _ -> typeMismatch "Review" json
