{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Scheduling.Mail (generateEmails) where

import Prelude hiding (group)
import Text.Printf (printf, PrintfArg(..), fmtPrecision, fmtChar, errorBadFormat, formatString, vFmt, IsChar)
import Control.Lens ((^.), (.~), (<&>), set, view, makeLenses, makeFields)
import Control.Scheduling.State
import Control.Scheduling.Person
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust, catMaybes)
import Control.Monad (liftM)
import Control.Lens hiding (zoom)
import Data.ByteString.Lazy (ByteString)
import Debug.Trace (traceShowId)
import Network.Mail.Mime
import Control.Scheduling.TimeSpan
import Data.Time.Clock
import Data.Time.Format
import Data.List (sortOn, sort)

-- add prospect bios

formatTimeSpan range = Text.pack $ (formatTime defaultTimeLocale "%a, %b %e at %R (" s) ++ ("30-60") ++ " minutes)"
  where
    s = (head range)^.start
    e = utctDayTime $ (last range)^.end
    l = length range * 30
    
formatJhuFacMeeting (p, ms) = do
  let name = Text.unwords [p^.firstName, p^.lastName]
      filename = Text.unpack name ++ ".eml"
      to = Address (Just name) (fromJust $ p^.email)
      from = Address (Just "Tom Lippincott") "tom@cs.jhu.edu"
      grid' = map (\(time, (prosp, facs)) -> Text.unlines $ [Text.intercalate " " [prosp^.firstName, prosp^.lastName, Text.concat ["(", fromJust $ prosp^.email, ")"], formatTimeSpan time]] ++ (if length facs > 1 then [Text.unwords ["Along", "with", f^.firstName, f^.lastName, Text.concat ["(", fromJust (f^.email), ")"]] | f <- facs, f /= p] else []) ++ [Text.unwords ["Application link: ", fromJust $ prosp^.application]] ++ [Text.unwords ["Zoom link: ", fromJust $ (head facs)^.zoom]]) ((sortOn (\(ts, _) -> head ts) . Map.toList) (Map.filter (\x -> length x > 0) ms))
      grid = Text.unlines grid'
  email <- renderMail' $ simpleMail' to from "Your CLSP visit schedule" (
    LText.fromStrict $ Text.unlines [ Text.concat ["Hi ", p^.firstName, ","]        
                                    , ""
                                    , "Your schedule (in EST) is below, along with information about any shared meetings, contact emails, the Zoom link (probably yours) to use, and a link to the prospect's application on Slate.  There are also three events scheduled for the week:"
                                    , ""
                                    , "  Monday, 5PM-6PM: Introduction to the CLSP"
                                    , overview
                                    , ""
                                    , "  Wednesday, 5PM-8PM: Poster session and mixer"
                                    , poster
                                    , ""
                                    , "  Friday, 5PM-8PM: Shared meal and game night"
                                    , shared
                                    , ""
                                    , "If you plan to join us for the shared meal, please fill in this poll so we can handle reimbursement:"
                                    , ""
                                    , doordash
                                    , ""
                                    , "We have also set up a Slack workspace for the week, feel free to join and interact with prospects, help answer questions, etc:"
                                    , ""
                                    , slack
                                    , ""
                                    , "-Tom"
                                    , ""
                                    , ""
                                    , grid
                                    ]
    )
  return $ if length grid' > 0 then Just (filename, email) else Nothing

overview = "  https://jh.zoom.us/j/91487972275?pwd=V0V5Vy9zczloMlV5Umo3Q1lLc2xXZz09"
poster = "  https://gather.town/app/l8nJEc8iysj09hEC/CLSP (password: 'c15pvisit')"
doordash = "  https://forms.gle/4ZBRcic4awFi5zBi7"
games = "  https://forms.gle/AL6e8ENroDVqJHGp7"
slack = "  https://join.slack.com/t/jhu-mq46074/shared_invite/zt-niwde4sh-BqI1W7VcfFFXOS33V~9aWg"
shared = "  https://jh.zoom.us/j/99013636868?pwd=YllTdWRyUGJVOGpWTVl5M1J3bENhdz09"
neighborhoods = "  URL"
eisner = "  Eisner, TIME, URL"
vandurme = "  March 12, 10:45-11:45, https://jhubluejays.zoom.us/j/97190315377"

formatNonJhuFacMeeting (p, ms) = do
  let name = Text.unwords [p^.firstName, p^.lastName]
      filename = Text.unpack name ++ ".eml"
      to = Address (Just name) (fromJust $ p^.email)
      from = Address (Just "Tom Lippincott") "tom@cs.jhu.edu"
      grid' = map (\(time, (prosp, facs)) -> Text.unlines $ [Text.intercalate " " [prosp^.firstName, prosp^.lastName, Text.concat ["(", fromJust $ prosp^.email, ")"], formatTimeSpan time]] ++ (if length facs > 1 then [Text.unwords ["Along", "with", f^.firstName, f^.lastName, Text.concat ["(", fromJust (f^.email), ")"]] | f <- facs, f /= p] else []) ++ [Text.unwords ["Zoom link: ", fromJust $ (head facs)^.zoom]]) ((sortOn (\(ts, _) -> head ts) . Map.toList) (Map.filter (\x -> length x > 0) ms))
      grid = Text.unlines grid'
  email <- renderMail' $ simpleMail' to from "Your CLSP visit schedule" (
    LText.fromStrict $ Text.unlines [ Text.concat ["Hi ", p^.firstName, ","]        
                                    , ""
                                    , "Your schedule (in EST) is below, along with information about any shared meetings, contact emails, and the Zoom link to use.  There are also three events scheduled for the week:"
                                    , ""
                                    , "  Monday, 5PM-6PM: Introduction to the CLSP"
                                    , overview
                                    , ""
                                    , "  Wednesday, 5PM-8PM: Poster session and mixer"
                                    , poster
                                    , ""
                                    , "  Friday, 5PM-8PM: Shared meal and game night"
                                    , shared
                                    , ""
                                    , "If you plan to join us for the shared meal, please fill in this poll so we can handle reimbursement:"
                                    , ""
                                    , doordash
                                    , ""
                                    , "We have also set up a Slack workspace for the week, feel free to join and interact with prospects, help answer questions, etc:"
                                    , ""
                                    , slack
                                    , ""
                                    , "-Tom"
                                    , ""
                                    , ""
                                    , grid
                                    ]
    )
  return $ if length grid' > 0 then Just (filename, email) else Nothing    
--  return (filename, email)


formatNonFacMeeting = formatNonJhuFacMeeting


formatProspMeeting (p, ms) = do
  let name = Text.unwords [p^.firstName, p^.lastName]
      filename = Text.unpack name ++ ".eml"
      to = Address (Just name) (fromJust $ p^.email)
      from = Address (Just "Tom Lippincott") "tom@cs.jhu.edu"
      grid' = map (\(time, facs) -> Text.unlines [Text.unwords [Text.intercalate " and " [
                                                                                 Text.intercalate " " [fac^.firstName, fac^.lastName, Text.concat ["(", fromJust (fac^.email), ")"]] | fac <- facs], formatTimeSpan time], Text.unwords ["Zoom link: ", fromJust $ (head facs)^.zoom]]) ((sortOn (\(ts, _) -> head ts) . Map.toList) (Map.filter (\x -> length x > 0) ms))
      grid = Text.unlines grid'
  email <- renderMail' $ simpleMail' to from "Your CLSP visit schedule" (
    LText.fromStrict $ Text.unlines [ Text.concat ["Dear ", p^.firstName, ","]        
                                    , ""
                                    , "Your schedule (in EST) is below, along with the Zoom link to use and contact emails in case of difficulty connecting.  Each meeting may be with one or two students or faculty and last from 30 to 60 minutes.  Additionally, there are three events scheduled for the week that you're encouraged to attend:"
                                    , ""
                                    , "  Monday, 5PM-6PM: Introduction to the CLSP"
                                    , overview
                                    , ""
                                    , "  Wednesday, 5PM-8PM: Poster session and mixer"
                                    , poster
                                    , ""
                                    , "  Friday, 5PM-8PM: Shared meal and game night"
                                    , shared
                                    , ""
                                    , "If you plan to join us for the shared meal, please fill in this poll so we can handle reimbursement:"
                                    , ""
                                    , doordash
                                    , ""
                                    , "For the games following the meal, please fill in your preference so we can plan for the right numbers:"
                                    , ""
                                    , games
                                    , ""                                    
                                    -- , "Our current students have put together short video tours of popular Baltimore neighborhoods:"
                                    -- , ""
                                    -- , neighborhoods
                                    -- , ""
                                    , "A Slack workspace will be open for the entire week to ask questions asynchronously, chat informally with current students and faculty, etc:"
                                    , ""
                                    , slack
                                    , ""
                                    , "Finally, Professor Van Durme has kindly offered for you to sit in on his reading group for some representative immersion in CLSP activities, should your schedule allow:"
                                    , ""
                                    , vandurme
                                    , ""
                                    , "We look forward to seeing you over the coming days!"
                                    , ""
                                    , "-The CLSP"
                                    , ""
                                    , ""
                                    , grid
                                    ]
    )
  return $ if length grid' > 0 then Just (filename, email) else error "WHAT?"
  --return (filename, email)


combineProspectMeetings :: (Ord a) => Map Person (Map Person a) -> Map Person (Map a [Person])
combineProspectMeetings ps = Map.map combine' ps
  where
    combine' p = Map.fromListWith (\a b -> sort $ a ++ b) $ (map (\(a, b) -> (b, [a])) . Map.toList) p


combineInterviewerMeetings :: (Ord a) => Map Person (Map Person a) -> Map Person (Map a (Person, [Person]))
combineInterviewerMeetings facs = Map.mapWithKey combine' facs
  where
    combine' fac mtgs = Map.fromList $ map gather' (Map.toList mtgs)
      where
        gather' (prosp, ts) = (ts, (prosp, allFacs))
          where
            allFacs = sort $ [f | (f, m) <- Map.toList facs, (prosp `Map.lookup` m) == (Just ts)]

generateEmails :: String -> String -> String -> State -> IO [(String, ByteString)]
generateEmails prosp jhuFac nonJhuFac state = do
  let meetings = Map.map (Map.mapKeys (\(f, l) -> head [p & availability .~ Nothing | p <- state^.prospects, p^.firstName == f && p^.lastName == l])) (Map.mapKeys (\(f, l) -> head [p | p <- state^.faculty, p^.firstName == f && p^.lastName == l]) (fromJust $ (state^.individualMeetings)))
      --revMeetings = invertMap meetings
      prospMeetings = (combineProspectMeetings . invertMap) meetings
      (jhuFacMeetings, nonJhuFacMeetings) = Map.partitionWithKey (\k v -> k^.group == (Just "Faculty")) (combineInterviewerMeetings meetings)
      --(nonFacMeetings, nonJhuFacMeetings) = Map.partitionWithKey (\k v -> k^.group == (Just "Student")) rest
      --facMeetings = meetings
  --print meetings
  --print $ map (\x -> x^.group) (Map.keys jhuFacMeetings)
  prospMeetings' <- sequence $ map formatProspMeeting (Map.toList prospMeetings)
  jhuFacMeetings' <- sequence $ map formatJhuFacMeeting (Map.toList jhuFacMeetings)
  nonJhuFacMeetings' <- sequence $ map formatNonJhuFacMeeting (Map.toList nonJhuFacMeetings)
  --nonFacMeetings' <- sequence $ map formatNonFacMeeting (Map.toList nonFacMeetings)
  let prospMeetings'' = map (\(n, m) -> (prosp ++ n, m)) (catMaybes prospMeetings')
      jhuFacMeetings'' = map (\(n, m) -> (jhuFac ++ n, m)) (catMaybes jhuFacMeetings')
      nonJhuFacMeetings'' = map (\(n, m) -> (nonJhuFac ++ n, m)) (catMaybes nonJhuFacMeetings')
      --nonFacMeetings'' = map (\(n, m) -> (nonFac ++ n, m)) nonFacMeetings'
  return $ prospMeetings'' ++ jhuFacMeetings'' ++ nonJhuFacMeetings''


invertMap m = Map.map (Map.fromList) unflat 
  where
    flat = concat $ map (\(f, v) -> [(p, [(f, t)]) | (p, t) <- Map.toList v]) (Map.toList m)
    unflat = Map.fromListWith (++) flat
