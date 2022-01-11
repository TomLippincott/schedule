{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Text (Text)
import Options.Generic (Generic, ParseRecord, Unwrapped, Wrapped, unwrapRecord, (:::), type (<?>)(..))
import Data.Text (Text)
import System.IO (stdout)
import Data.Maybe (fromMaybe, catMaybes, fromJust)
import Text.Printf (printf)
import Control.Monad
import Control.Scheduling
import Data.Time
import Data.Time.Format
import Control.Lens hiding (Wrapped, Unwrapped)
import qualified Data.ByteString.Lazy as BS
import Debug.Trace (traceShowId)
--import Control.Monad.Log
import qualified Network.Google.Sheets as S
import Network.Google.Resource.Sheets.Spreadsheets.Get
import Network.Google.Sheets hiding (Text, sheet)

sheetName :: Sheet -> Maybe Text
sheetName s = join $ s ^. sProperties <&> view sTitle

events = [ ("Welcome", ("Mon 08:00", "Mon 09:00"))
         , ("Shared lunch", ("Wed 12:00", "Wed 13:00"))
         ]

data Args w = Init { access :: w ::: Maybe Text <?> "Access code for Google Sheets endpoint"
                   , force :: w ::: Bool <?> "If set, existing tables will be overwritten"
                   , windows :: w ::: [String] <?> "Time windows (can specify multiple times): format is 'YYYY-MM-DD HH:MM-HH:MM'"
                   --, granularity :: w ::: Maybe Integer <?> "Time granularity, in minutes, for scheduling"
                   }
            | Show { access :: w ::: Maybe Text <?> "Access code for Google Sheets endpoint"
                   }
            | Solve { access :: w ::: Maybe Text <?> "Access code for Google Sheets endpoint"
                    , force :: w ::: Bool <?> "If set, existing tables will be overwritten"
                    }
            | Emails { access :: w ::: Maybe Text <?> "Access code for Google Sheets endpoint"
                     , jhuFacultyEmails :: w ::: String <?> "Path to write JHU faculty emails"
                     , nonJhuFacultyEmails :: w ::: String <?> "Path to write non-JHU-faculty emails"
                     --, nonFacultyEmails :: w ::: String <?> "Path to write non-faculty emails"
                     , prospectEmails :: w ::: String <?> "Path to write prospect emails"
                     }
  deriving (Generic)                             
                                                            
instance ParseRecord (Args Wrapped)
deriving instance Show (Args Unwrapped)

-- add resources!

generateSlots gran wins = do
  let dates = ["2021-03-" ++ (show d) | d <- [8,9,10,11,12]]
  toDrop <- sequence $ [] -- stringsToSlots "2021-03-08" "2021-03-08" "17:00" "17:00" gran
                       --, stringsToSlots "2021-03-10" "2021-03-10" "17:00" "19:00" gran
                       --, stringsToSlots "2021-03-12" "2021-03-12" "17:00" "19:00" gran
                       --]
  slots <- sequence $ [stringsToSlots d d "08:00" "19:00" gran | d <- dates]
  let toDrop' = concat toDrop
      slots' = [s | s <- concat slots, not $ s `elem` toDrop']
  return slots'

getCurrentCount :: State -> Map (Text, Text) Int
getCurrentCount state = Map.fromListWith (+) (pro ++ base)
  where
    fac = (Map.toList . fromMaybe Map.empty) $ state^.individualMeetings
    base = map (\p -> ((p^.firstName, p^.lastName), 0)) (state ^. prospects)
    mts = concat $ map (Map.toList . snd) fac
    pro = map ((,1) . fst) mts

getCurrentCount' :: State -> [((Text, Text), [(Text, Text)])]
getCurrentCount' state = fac --Map.fromListWith (+) (pro ++ base)
  where
    fac = (map (\(k, v) -> (k, Map.keys v)) . Map.toList . fromMaybe Map.empty) $ state^.individualMeetings
    --base = map (\p -> ((p^.firstName, p^.lastName), Map.size $ p^.)) (state ^. faculty)
    --mts = concat $ map (Map.toList . snd) fac
    --pro = map ((,1) . fst) mts


main :: IO ()
main = do
  ps <- unwrapRecord "" :: IO (Args Unwrapped)
  let sid = fromMaybe "NULL" (access ps)
  ssheet <- readSpreadsheet sid
  let state = readState (generateSlots 60 []) ssheet
  case ps of Init{..} -> do
               let slu = ssheet^.sheetLookup
                   state' = state { _slots=generateSlots 60 [] }
               if ((slu Map.!? "Faculty Preferences") == Nothing && (slu Map.!? "Faculty Availability") == Nothing) || force == True
                 then writeForms sid (slu Map.!? "Faculty Availability") (slu Map.!? "Faculty Preferences") state'
                 else putStrLn "Refusing to overwrite existing forms (you may specify '--force' if you're sure)"
             Show{..} -> do
               --let mtg = (Map.toList . Map.map (\x -> length $ Map.keys x) . fromJust) $ state^.individualMeetings
               --putStrLn $ (show . sum . map snd) mtg
               --putStrLn $ unlines (map show mtg)
               putStrLn $ show $ (state^.individualMeetings)
               putStrLn $ unlines $ map show (getCurrentCount' state)
               --print $ getCurrentCount state
               return ()
               --putStrLn $ show $ (head $ state^.faculty) ^.availability
               --putStrLn $ show $ state^.faculty
               --putStrLn $ show $ state^.requestedMeetings
               --putStrLn $ show $ state^.individualMeetings
               --printSchedules (compressSlots state)
             Solve{..} -> do
               let slu = ssheet^.sheetLookup
                   state' = state { _slots=generateSlots 60 [] }
               --let state' = compressSlots $ state { _slots= Just $ [s | s <- (fromMaybe [] (defaultSlots 60))]}
               state'' <- solveSchedule state'
               if ((slu Map.!? "Interviewer Schedule") == Nothing && (slu Map.!? "Prospect Schedule" == Nothing)) || force == True
                 then writeSchedule sid (slu Map.!? "Interviewer Schedule") (slu Map.!? "Prospect Schedule") state'' (fromJust $ generateSlots 60 [])
                 else putStrLn "Refusing to overwrite existing schedule (you may specify '--force' if you're sure)"

               --print $ getCurrentCount state''
               --print $ map (\(x, y) -> (x, length y)) (getCurrentCount' state'')
               --putStrLn $ unlines $ map (show . 
               return ()
             Emails{..} -> do
               emails <- generateEmails prospectEmails jhuFacultyEmails nonJhuFacultyEmails state
               sequence $ map (\(f, m) -> BS.writeFile f m) emails
               return ()
