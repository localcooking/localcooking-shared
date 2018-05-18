{-# LANGUAGE
    OverloadedStrings
  , TemplateHaskell
  , DeriveGeneric
  #-}

module LocalCooking.Common.Rating where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (String))
import Data.Aeson.Attoparsec (attoAeson)
import Data.Attoparsec.Text (Parser, string)
import Data.Hashable (Hashable)
import Control.Applicative ((<|>))
import Database.Persist.TH (derivePersistField)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)


data Rating
  = ZeroStar
  | HalfStar
  | OneStar
  | OneHalfStar
  | TwoStar
  | TwoHalfStar
  | ThreeStar
  | ThreeHalfStar
  | FourStar
  | FourHalfStar
  | FiveStar
  deriving (Eq, Ord, Enum, Show, Read, Generic)
derivePersistField "Rating"

instance Arbitrary Rating where
  arbitrary = oneof
    [ pure ZeroStar
    , pure HalfStar
    , pure OneStar
    , pure OneHalfStar
    , pure TwoStar
    , pure TwoHalfStar
    , pure ThreeStar
    , pure ThreeHalfStar
    , pure FourStar
    , pure FourHalfStar
    , pure FiveStar
    ]

instance Hashable Rating

instance ToJSON Rating where
  toJSON x = String $ case x of
    ZeroStar -> "0"
    HalfStar -> "1/2"
    OneStar -> "1"
    OneHalfStar -> "3/2"
    TwoStar -> "2"
    TwoHalfStar -> "5/2"
    ThreeStar -> "3"
    ThreeHalfStar -> "7/2"
    FourStar -> "4"
    FourHalfStar -> "9/2"
    FiveStar -> "5"

instance FromJSON Rating where
  parseJSON = attoAeson ratingParser

ratingParser :: Parser Rating
ratingParser = do
  let zero = ZeroStar <$ string "0"
      half = HalfStar <$ string "1/2"
      one = OneStar <$ string "1"
      onehalf = OneHalfStar <$ string "3/2"
      two = TwoStar <$ string "2"
      twohalf = TwoHalfStar <$ string "5/2"
      three = ThreeStar <$ string "3"
      threehalf = ThreeHalfStar <$ string "7/2"
      four = FourStar <$ string "4"
      fourhalf = FourHalfStar <$ string "9/2"
      five = FiveStar <$ string "5"
  zero <|> half <|> one <|> onehalf <|> two <|> twohalf <|> three <|> threehalf <|> four <|> fourhalf <|> five
