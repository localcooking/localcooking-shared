{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  #-}

module LocalCooking.Common.Chef where

import LocalCooking.Common.User.Name (Name)
import LocalCooking.Common.Tag.Chef (ChefTag)
import LocalCooking.Common.Rating (Rating)

import Data.Image.Source (ImageSource)


data ChefSynopsis = ChefSynopsis
  { chefSynopsisName   :: Name
  , chefSynopsisImage  :: ImageSource
  , chefSynopsisRating :: Rating
  , chefSynopsisOrders :: Int
  , chefSynopsisTags   :: [ChefTag]
  }
