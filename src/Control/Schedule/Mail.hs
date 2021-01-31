{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Schedule.Mail (generateEmails) where

import Text.Printf (printf, PrintfArg(..), fmtPrecision, fmtChar, errorBadFormat, formatString, vFmt, IsChar)
import Control.Lens ((^.), (.~), (<&>), set, view, makeLenses, makeFields)
import Control.Schedule.State
import Control.Schedule.Person
import Data.Text (Text)
import qualified Data.Text as Text
--import Data.Text (Text)
import qualified Data.Text.Lazy as LText
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust)
import Control.Monad (liftM)
import Control.Lens
import Data.ByteString.Lazy (ByteString)
import Debug.Trace (traceShowId)
import Network.Mail.Mime
import Control.Schedule.TimeSpan
import Data.Time.Clock
import Data.Time.Format
import Data.List (sortOn)

formatTimeSpan range = Text.pack $ (formatTime defaultTimeLocale "%a, %b %e at %R (" s) ++ (show l) ++ " minutes)"
  where
    s = (head range)^.start
    e = utctDayTime $ (last range)^.end
    l = length range * 30
    
formatFacMeeting (p, ms) = do
  let name = Text.unwords [p^.firstName, p^.lastName]
      filename = Text.unpack name ++ ".eml"
      to = Address (Just name) (fromJust $ p^.email)
      --from = Address (Just "Ruth Scally") "rscally1@jhu.edu"
      from = Address (Just "Tom Lippincott") "tom@cs.jhu.edu"
      grid = Text.unlines $ map (\(prosp, time) -> Text.unlines [Text.intercalate " " [prosp^.firstName, prosp^.lastName, fromJust $ prosp^.email, formatTimeSpan time], Text.unwords ["Application link: ", fromJust $ prosp^.application]]) ((sortOn (\(_, ts) -> head ts) . Map.toList) (Map.filter (\x -> length x > 0) ms))
  email <- renderMail' $ simpleMail' to from "Your (corrected) interview schedule" (LText.fromStrict $ Text.unlines [Text.concat ["Hi ", p^.firstName, ","], "", "Your *corrected* interview schedule is below: again, apologies for the mistake, these have been sanity-checked a couple orthogonal ways, so fingers crossed.  I'll wait an hour just in case anyone sees glaring problems (maybe missing Zoom links will be sent by then, too?) and then pass the schedules along to the prospects.", "", "", "-Tom", "", "", grid])
  return (filename, email)

formatProspMeeting (p, ms) = do
  let name = Text.unwords [p^.firstName, p^.lastName]
      filename = Text.unpack name ++ ".eml"
      to = Address (Just name) (fromJust $ p^.email)
      from = Address (Just "Tom Lippincott") "tom@cs.jhu.edu"
      --from = Address (Just "Ruth Scally") "rscally1@jhu.edu"
      grid = Text.unlines $ map (\(fac, time) -> Text.unlines [Text.unwords [fac^.firstName, fac^.lastName, formatTimeSpan time], Text.unwords ["Zoom link: ", fromJust $ fac^.application]]) ((sortOn (\(_, ts) -> head ts) . Map.toList) (Map.filter (\x -> length x > 0) ms))
  email <- renderMail' $ simpleMail' to from "Your (corrected) interview schedule with the Center for Language and Speech Processing" (LText.fromStrict $ Text.unlines [Text.concat ["Dear ", p^.firstName, ","], "", "Apologies for the revision, an error in the constraint-satisfaction code led to quite a few spurious assignments.  Below is your *corrected* itinerary, including Zoom links to use for each interview (if 'TBD', the information will be sent shortly).  Times are EST.  In most cases, you can learn more about your interviewers at:", "", "  https://www.clsp.jhu.edu/faculty/", "", "We look forward to meeting you!", "", "-The CLSP", "", "", grid])
  return (filename, email)

em = "  https://docs.google.com/document/d/1e0ND29CLVLP2MvrJxfVP1sL_mqka7Q2y51amZX97fwg/edit"
    
generateEmails :: String -> String -> State -> IO [(String, ByteString)]
generateEmails prosp fac state = do
  let facMeetings = Map.map (Map.mapKeys (\(f, l) -> head [p & availability .~ Nothing | p <- state^.prospects, p^.firstName == f && p^.lastName == l])) (Map.mapKeys (\(f, l) -> head [p | p <- state^.faculty, p^.firstName == f && p^.lastName == l]) (fromJust $ (state^.individualMeetings)))
      prospMeetings = invertMap facMeetings
  facMeetings' <- sequence $ map formatFacMeeting (Map.toList facMeetings)
  prospMeetings' <- sequence $ map formatProspMeeting (Map.toList prospMeetings)
  let facMeetings'' = map (\(n, m) -> (fac ++ n, m)) facMeetings'
      prospMeetings'' = map (\(n, m) -> (prosp ++ n, m)) prospMeetings'
  return $ facMeetings'' ++ prospMeetings''


invertMap m = Map.map (Map.fromList) unflat 
  where
    flat = concat $ map (\(f, v) -> [(p, [(f, t)]) | (p, t) <- Map.toList v]) (Map.toList m)
    unflat = Map.fromListWith (++) flat
