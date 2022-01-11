{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Scheduling.TimeSpan (expand, TimeSpan(..), start, end, stringsToSlots, timesFromStrings) where

import Data.Time
import Control.Lens ((^.), (.~), (<&>), set, view, makeLenses, makeFields)
import Data.List (unfoldr)

data TimeSpan = TimeSpan { _start :: UTCTime
                         , _end :: UTCTime
                         } deriving (Show, Eq, Ord)

makeLenses ''TimeSpan

stringsToSlots :: String -> String -> String -> String -> Integer -> Maybe [TimeSpan]
stringsToSlots sd ed st et sl = Just slots
  where
    days = daysFromStrings sd ed
    (st', et') = timesFromStrings st et
    ranges = [TimeSpan (UTCTime d st') (UTCTime d et') | d <- days]
    increment' = secondsToDiffTime $ 60 * sl :: DiffTime
    slots = concat $ map (expand increment') ranges

expand :: DiffTime -> TimeSpan -> [TimeSpan]
expand slotLength block = if s' > e then [n] else n:(expand slotLength block')
  where
    s = block ^. start
    e = block ^. end
    s' = s { utctDayTime=utctDayTime s + slotLength }
    n = TimeSpan s s'
    block' = TimeSpan s' e
  
daysFromStrings :: String -> String -> [Day]
daysFromStrings s e = ds
  where
    s' = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" s :: Day
    e' = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" e :: Day
    ds = unfoldr (\d -> if d > e' then Nothing else Just (d, (addDays 1 d))) s'

timesFromStrings :: String -> String -> (DiffTime, DiffTime)
timesFromStrings s e = (timeOfDayToTime s', timeOfDayToTime e')
  where
    s' = parseTimeOrError True defaultTimeLocale "%R" s :: TimeOfDay
    e' = parseTimeOrError True defaultTimeLocale "%R" e :: TimeOfDay

