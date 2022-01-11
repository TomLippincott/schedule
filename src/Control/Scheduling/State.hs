{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Scheduling.State (State(..), individualMeetings, groupMeetings, faculty, prospects, slots, granularity, compressSlots, simpleState, requestedMeetings) where

import Text.Printf (printf, PrintfArg(..), fmtPrecision, fmtChar, errorBadFormat, formatString, vFmt, IsChar)
import Control.Lens ((^.), (.~), (&), (<&>), set, view, makeLenses, makeFields, (%~))
import Control.Scheduling.Person (Person(..), availability)
import Control.Scheduling.TimeSpan (TimeSpan(..))
import Control.Scheduling.Preference (Preference(..))
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (intercalate)
import Data.Maybe (fromMaybe, fromJust)
import System.Random
import qualified Control.Monad.State as S

data State = State { _faculty :: [Person]
                   , _prospects :: [Person]
                   , _requestedMeetings :: Maybe [Preference]
                   , _slots :: Maybe [TimeSpan]
                   , _individualMeetings :: Maybe (Map (Text, Text) (Map (Text, Text) [TimeSpan])) -- faculty to student
                   , _groupMeetings :: Maybe (Map (Text, Text) (Map [(Text, Text)] [TimeSpan]))
                   , _granularity :: Maybe Int
                   , _interviewerSheet :: Text
                   , _prospectSheet :: Text
                   , _interviewerAvailabilitySheet :: Text
                   , _prospectAvailabilitySheet :: Text
                   , _individualPreferenceSheet :: Text
                   , _groupPreferenceSheet :: Text
                   , _interviewerScheduleSheet :: Text
                   , _prospectScheduleSheet :: Text
                   } deriving (Show)

instance PrintfArg State where
  formatArg State{..} fmt | fmtChar (vFmt 'P' fmt) == 'P' = formatString (printf "State:\n\n  Faculty:\n%s\n\n  Prospects:\n%s\n\n  Slots:\n%s\n " facs prosps slots :: String) (fmt { fmtChar = 's', fmtPrecision = Nothing })
    where
      facs = intercalate "\n" (map (printf "    %s") _faculty :: [String])
      prosps = intercalate "\n" (map (printf "    %s") _prospects :: [String])
      slots = printf "    %d" ((length . fromMaybe []) _slots :: Int) :: String

makeLenses ''State

simpleState = State [] [] Nothing Nothing Nothing Nothing Nothing "Interviewers" "Prospects" "Interviewer Availability" "Prospect Availability" "Individual Preferences" "Group Preferences" "Interviewer Schedule" "Prospect Schedule"

filtAvail :: (Ord a) => Set a -> Maybe [a] -> Maybe [a]
filtAvail keep others = Just $ Set.toList toKeep
  where
    others' = (Set.fromList . fromJust) others
    toKeep = Set.intersection others' keep

compressSlots :: State -> State
compressSlots state@(State{..}) = state & faculty .~ faculty' & prospects .~ prospects' & slots .~ slots'
  where
    allFacAvail = Set.fromList $ concat [fromJust $ f ^. availability | f <- _faculty]
    allProspAvail = Set.fromList $ concat [fromJust $ f ^. availability | f <- _prospects]
    allSlots = Set.fromList $ fromJust _slots
    overlap' = Set.intersection allFacAvail allProspAvail
    overlap = Set.intersection overlap' allSlots
    faculty' = [p & availability %~ filtAvail overlap | p <- _faculty]
    prospects' = [p & availability %~ filtAvail overlap | p <- _prospects]
    slots' = Just $ Set.toList overlap
-- randomizeEntries :: State -> IO State
-- randomizeEntries s = do
--   faculty' <- randomizeAvailability (_faculty s) (_slots s)
--   faculty'' <- sequence $ map (randomizePreferences (_prospects s)) faculty'
--   return $ s { _faculty=faculty'' } 

-- randomizePreferences :: [Person] -> Person -> IO Person
-- randomizePreferences prosp fac = do
--   r <- newStdGen
--   let rs = randomRs (0.0, 1.0) r :: [Double]
--       bs = map (\v -> if v < 0.5 then 0 else if v < 0.85 then 1 else 1) rs
--   return $ fac { _preferences=Just $ Map.fromList $ zip prosp bs }
  
-- randomizeAvailability :: [Person] -> Maybe [TimeSpan] -> IO [Person]
-- randomizeAvailability fac slots = do
--   return $ [p { _availability=slots }| p <- fac]
