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
import Control.Schedule
import Data.Time
import Data.Time.Format
import Control.Lens hiding (Wrapped, Unwrapped)
import qualified Data.ByteString.Lazy as BS
import Debug.Trace (traceShowId)
--import Control.Monad.Log

data Args w = Init { access :: w ::: Maybe Text <?> "Access code for Google Sheets endpoint"
                   , force :: w ::: Bool <?> "If set, existing tables will be overwritten"
                   , windows :: w ::: [String] <?> "Time windows (can specify multiple times): format is 'YYYY-MM-DD HH:MM-HH:MM'"
                   , granularity :: w ::: Maybe Integer <?> "Time granularity, in minutes, for scheduling"
                   }
            | Show { access :: w ::: Maybe Text <?> "Access code for Google Sheets endpoint"
                   }
            | Solve { access :: w ::: Maybe Text <?> "Access code for Google Sheets endpoint"
                    , force :: w ::: Bool <?> "If set, existing tables will be overwritten"
                    }
            | Emails { access :: w ::: Maybe Text <?> "Access code for Google Sheets endpoint"
                     , faculty :: w ::: String <?> "Path to write faculty emails"
                     , prospect :: w ::: String <?> "Path to write prospect emails"
                     }
  deriving (Generic)                             
                                                            
instance ParseRecord (Args Wrapped)
deriving instance Show (Args Unwrapped)

generateSlots gran wins = do
  let dates = ["2021-02-" ++ (show d) | d <- [1,2,3,4,5,6]]
  slots <- sequence $ [stringsToSlots d d "00:00" "23:30" gran | d <- dates]
  return $ concat slots


main :: IO ()
main = do
  ps <- unwrapRecord "" :: IO (Args Unwrapped)
  let sid = fromMaybe "1TCsdkaKyyAeGU5hRht_wfDKxJJZNgnyRl1su48qZ1-A" (access ps)
  ssheet <- readSpreadsheet sid
  let state = readState [] ssheet      
  case ps of Init{..} -> do
               let slu = ssheet^.sheetLookup
                   state' = state { _slots=generateSlots (fromMaybe 30 granularity) }
               if ((slu Map.!? "Faculty Preferences") == Nothing && (slu Map.!? "Faculty Availability") == Nothing) || force == True
                 then writeForms sid (slu Map.!? "Faculty Availability") (slu Map.!? "Faculty Preferences") state'
                 else putStrLn "Refusing to overwrite existing forms (you may specify '--force' if you're sure)"
             Show{..} -> do
               printSchedules (compressSlots state)
             Solve{..} -> do
               let state' = compressSlots $ state { _slots= Just $ [s | s <- (fromMaybe [] (defaultSlots 30))] }
               state'' <- solveSchedule state'
               if ((slu Map.!? "Faculty Schedule") == Nothing && (slu Map.!? "Prospect Schedule" == Nothing)) || force == True
                 then writeSchedule sid (slu Map.!? "Faculty Schedule") (slu Map.!? "Prospect Schedule") state'' (fromJust $ defaultSlots 30)
                 else putStrLn "Refusing to overwrite existing schedule (you may specify '--force' if you're sure)"
               let mtg = (Map.toList . Map.map (\x -> length $ Map.keys x) . fromJust) $ state''^.individualMeetings
               putStrLn $ (show . sum . map snd) mtg
               putStrLn $ unlines (map show mtg)
               return ()
             Emails{..} -> do
               emails <- generateEmails prospect faculty state
               sequence $ map (\(f, m) -> BS.writeFile f m) emails
               return ()
