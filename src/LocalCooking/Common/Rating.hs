{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  #-}

module LocalCooking.Common.Rating where


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
