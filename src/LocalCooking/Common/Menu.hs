{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  , DeriveGeneric
  #-}

module LocalCooking.Common.Menu where

import LocalCooking.Common.Meal (Meal)

import Data.Time (UTCTime)
import Data.Text.Markdown (MarkdownText)


data Menu = Menu
  { menuPublished   :: Maybe UTCTime
  , menuDeadline    :: UTCTime
  , menuDescription :: MarkdownText
  , menuAuthor      :: ChefSynopsis
  , menuMeals       :: [Meal]
  }
