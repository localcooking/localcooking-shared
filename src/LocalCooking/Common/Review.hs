{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module LocalCooking.Common.Review where

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


data Review = Review
  { reviewRating  :: Rating
  , reviewHeading :: Text
  , reviewBody    :: MarkdownText
  , reviewImages  :: [ImageSource]
  } deriving (Eq, Show, Generic)
