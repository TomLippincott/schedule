{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Control.Schedule.Solve (solveSchedule, printSchedules) where

import Data.SBV
import Data.SBV.Trans.Control
import Control.Schedule.State (State(..), individualMeetings, groupMeetings, faculty, prospects, requestedMeetings)
import Control.Schedule.Preference
import Control.Schedule.Person
import Data.Maybe (Maybe, fromJust, catMaybes, fromMaybe)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Schedule.Person (Person(..), availability, firstName, lastName, fullName, preferences)
import Control.Schedule.TimeSpan (TimeSpan(..), stringsToSlots, start, end)
import Text.Printf (printf)
import Data.Bimap (Bimap)
import qualified Data.Set as Set
import qualified Data.Bimap as Bimap
import Control.Monad.IO.Class (liftIO)
import qualified Data.SBV.List as L
import Text.Read
import Control.Lens
import Control.Monad
import Data.Text (Text)
import Debug.Trace (traceShowId)

type Vars = Map (Int, Int, Int) SBool
type GVars = Map ((Int, [Int]), Int) SBool
type SVars = Map (Int, Int) (SList Bool) -- [SBool]
type Maxes = (Int, Int, Int)
type Ctx = (Maxes, Bimap (Text, Text) Int, Bimap (Text, Text) Int, Bimap TimeSpan Int)
type GMR = [((Text, Text), [[(Text, Text)]])]

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = Map.toList (Map.fromListWith (+) [(x, 1) | x <- xs])

getCurrentCount :: State -> Map (Text, Text) Int
getCurrentCount state = Map.fromListWith (+) (pro ++ base)
  where
    fac = (Map.toList . fromMaybe Map.empty) $ state^.individualMeetings
    base = map (\p -> ((p^.firstName, p^.lastName), 0)) (state ^. prospects)
    mts = concat $ map (Map.toList . snd) fac
    pro = map ((,1) . fst) mts

validate state oldstate = if all (\(f,p,t) -> t `elem` f && t `elem` p) mtgs then "valid" else "invalid"
  where
    flu = Map.fromList $ map (\p -> ((p^.firstName, p^.lastName), fromJust $ p^.availability)) (oldstate^.faculty)
    plu = Map.fromList $ map (\p -> ((p^.firstName, p^.lastName), fromJust $ p^.availability)) (oldstate^.prospects)
    mtgs = concat $ (map (\(k, v) -> concat $ [[(flu Map.! k, plu Map.! p, t) | t <- ts] | (p, ts) <- Map.toList v]) . Map.toList . fromJust) $ state^.individualMeetings
    people = []
  
    
--
--
--
solveSchedule :: State -> IO State
solveSchedule state = do
  let state''' = state & individualMeetings .~ (Just Map.empty)
  --state' <- solveGroupMeetings state'''
  --state' <- solveScheduleTier state''' 1 Map.empty False
  state' <- solveSchedule' state'''
  -- let mtgs' = concat ((map Map.keys . Map.elems . fromMaybe Map.empty) $ state'' ^. individualMeetings)
  --     cts' = Map.fromListWith (+) (map (, 1) mtgs')
  --     pmaxes = Map.fromListWith (+) $ (map (,1) . map (\p -> (p^.firstName, p^.lastName)) . concat . map Map.keys . map (\f -> fromJust $ f^.preferences)) (state^.faculty)
      
  -- print cts'
  -- print $ (sum . map snd . Map.toList) cts'
  -- let target = 3
  --     cur = getCurrentCount state''
  --     needed = Map.mapWithKey (\k v -> max 0 ((max target (pmaxes Map.! k))- v)) cur
  -- print needed
  -- print $ (sum . map snd . Map.toList) needed
  -- print $ state'' ^. individualMeetings
  -- print $ validate state'' state


  -- state' <- solveScheduleTier state'' 1 needed True
  -- let bmtgs' = concat ((map Map.keys . Map.elems . fromMaybe Map.empty) $ state' ^. individualMeetings)
  --     bcts' = Map.fromListWith (+) (map (, 1) bmtgs')
  -- print bcts'
  -- print $ (sum . map snd . Map.toList) bcts'
  -- let target = 3
  --     bcur = getCurrentCount state'
  --     bneeded = Map.map (\v -> max 0 (target - v)) bcur
  -- print bneeded
  -- print $ (sum . map snd . Map.toList) bneeded
  -- print $ validate state' state

  -- print $ getCurrentCount state'


  -- --print $ [(p ^. firstName, p ^. lastName, (length . fromJust) (p ^. availability)) | p <- state' ^. prospects]
  -- --print $ [(p ^. firstName, p ^. lastName, (length . fromJust) (p ^. availability)) | p <- state' ^. faculty]
  -- let mtgs'' = concat ((map Map.keys . Map.elems . fromMaybe Map.empty) $ state' ^. individualMeetings)
  --     cts'' = Map.fromListWith (+) (map (, 1) mtgs'')
  -- print cts''
  
  --    state' = state''
  --state' <- solveScheduleTier state'' 1 Map.empty -- needed
  
  --let fac = state' ^. faculty
  --    n = 2
  --    state'' = state' & faculty .~ (map (unasked n (state' ^. prospects)) fac)
  --    sched = (map (Map.keys . snd) . Map.toList . fromJust) $ state' ^. individualMeetings
  --    counts = frequency $ concat sched
  --print $ state' ^. schedule
  --state''' <- solveScheduleTier state'' n

  --let state''''' = state'''
  -- let fac = state''' ^. faculty
  --     n = 1
  --     state'''' = state''' & faculty .~ (map (unasked n (state''' ^. prospects)) fac)
  --     sched = (map (Map.keys . snd) . Map.toList . fromJust) $ state''' ^. schedule
  --     counts = frequency $ concat sched
  -- print counts  
  -- state''''' <- solveScheduleTier state'' n

  
  --let sched = (map (Map.keys . snd) . Map.toList . fromJust) $ state''''' ^. schedule
  --    counts = frequency $ concat sched
  --print counts



  --printSchedules state'''''
  return state'

unasked :: Int -> [Person] -> Person -> Person
unasked n ps f = f & preferences ?~ prefs''
  where
    prefs' = [p ^. fullName | p <- (Map.keys . fromJust) $ f ^. preferences]
    prefs'' = Map.fromList [(p, n) | p <- ps, not $ elem (p ^. fullName) prefs'] 
                            --(k, v) <- Map.toList prefs', v == (-1)]

contiguous :: [TimeSpan] -> Bool
contiguous [] = True
contiguous (a:[]) = True
contiguous (a:b:rest) = (a ^. end) == (b ^. start) && (contiguous (b:rest))

collapseSlots :: Int -> [TimeSpan] -> Map TimeSpan [TimeSpan]
collapseSlots n ss = Map.fromList $ go ss []
  where
    go [] acc = acc
    go ss' acc = if contiguous s && length s == n then go e (tpl:acc) else go (tail ss') acc
      where
        (s, e) = splitAt n ss'
        s' = head s
        e' = last s
        tpl = (TimeSpan (s' ^. start) (e' ^. end), s)
        -- (s, e) = splitAt n ss'
        -- tpl = (TimeSpan ((head s) ^. start) ((last s) ^. end), s)
  
solveScheduleTier :: State -> Int -> Map (Text, Text) Int -> Bool -> IO State
solveScheduleTier state tier rem strict = do
  let sts = collapseSlots tier ((fromJust . _slots) state)
      facs = enblocken (_faculty state) sts
      prosps = enblocken (_prospects state) sts
      ctx@(_, _, plu, _) = makeContext facs prosps (Map.keys sts)
      rem' = Map.mapKeys (\k -> plu Bimap.! k) rem
  sol <- satWith z3 $ problem ctx facs prosps sts tier rem' strict state
  let sol' = getModelDictionary sol
  let iVars = Map.fromList $ map (\(k, v) -> (read k :: (Int, Int, Int), fromCV v :: Bool)) [(k, v) | (k, v) <- Map.toList sol', k /= "goal" && (not $ '[' `elem` k)]
  case sol of u@(SatResult (Unsatisfiable cfg cores)) -> print (u, cfg, cores)
              _ -> print $ "solved tier " ++ (show tier)
  return $ updateState state iVars ctx sts


solveSchedule' :: State -> IO State
solveSchedule' state = do
  let tier = 1
      sts = collapseSlots tier ((fromJust . _slots) state)
      facs = enblocken (_faculty state) sts
      prosps = enblocken (_prospects state) sts
      ctx@(_, _, plu, _) = makeContext facs prosps (Map.keys sts)
      rem' = Map.empty --Map.mapKeys (\k -> plu Bimap.! k) rem
      strict = True
  sol <- satWith z3 $ problem ctx facs prosps sts tier rem' strict state
  let sol' = getModelDictionary sol
  let iVars = Map.fromList $ map (\(k, v) -> (read k :: (Int, Int, Int), fromCV v :: Bool)) [(k, v) | (k, v) <- Map.toList sol', k /= "goal" && (not $ '[' `elem` k)]
  case sol of u@(SatResult (Unsatisfiable cfg cores)) -> print (u, cfg, cores)
              _ -> print $ "solved!"
  return $ updateState state iVars ctx sts


solveGroupMeetings :: State -> IO State
solveGroupMeetings state = do
  let tier = 1
      sts = collapseSlots tier ((fromJust . _slots) state)
      facs = enblocken (_faculty state) sts
      prosps = enblocken (_prospects state) sts
      ctx@(_, _, plu, _) = makeContext facs prosps (Map.keys sts)
      --rem' = Map.mapKeys (\k -> plu Bimap.! k) rem
  sol <- satWith z3 $ groupProblem ctx facs prosps sts tier state
  let sol' = getModelDictionary sol
  let iVars = Map.fromList $ map (\(k, v) -> (read k :: (Int, Int, Int), fromCV v :: Bool)) [(k, v) | (k, v) <- Map.toList sol', k /= "goal" && (not $ '[' `elem` k)]
  case sol of u@(SatResult (Unsatisfiable cfg cores)) -> print (u, cfg, cores)
              _ -> print $ "solved groups"
  return $ updateGroupMeetings state iVars ctx sts

updateGroupMeetings :: State -> Map (Int, Int, Int) Bool -> Ctx -> Map TimeSpan [TimeSpan] -> State
updateGroupMeetings state vars ctx sts = state

enblocken :: [Person] -> Map TimeSpan [TimeSpan] -> [Person]
enblocken facs lu = map go facs
  where
    go f = f & availability ?~ [k | (k, vs) <- Map.toList lu, all (\v -> elem v (fromJust $ f ^. availability)) vs]


updateState :: State -> Map (Int, Int, Int) Bool -> Ctx -> Map TimeSpan [TimeSpan] -> State
updateState state vars ctx timeLookup = state & faculty .~ fac' & prospects .~ prosp' & individualMeetings ?~ newSched
  where
    ((nFac, nProsp, nSlot), fLU, pLU, sLU) = ctx
    fac' = [updateAvailability f (concat [timeLookup Map.! (sLU Bimap.!> sid) | ((fid, pid, sid), b) <- Map.toList vars, b == True, fid == fLU Bimap.! (f ^. fullName)]) | f <- state ^. faculty]
    prosp' = [updateAvailability p (concat [timeLookup Map.! (sLU Bimap.!> sid) | ((fid, pid, sid), b) <- Map.toList vars, b == True, pid == pLU Bimap.! (p ^. fullName)]) | p <- state ^. prospects]
    oldSched = fromMaybe Map.empty (state ^. individualMeetings)
    newSched = Map.fromList [(f ^. fullName, updateFacultySchedule (Map.findWithDefault Map.empty (f ^. fullName) oldSched) [(pLU Bimap.!> pid, timeLookup Map.! (sLU Bimap.!> sid)) | ((fid, pid, sid), b) <- Map.toList vars, b == True, fid == fLU Bimap.! (f ^. fullName)] ) | f <- state ^. faculty]


-- Remove assigned time slots from faculty availability
updateAvailability :: Person -> [TimeSpan] -> Person
updateAvailability p ts = p & availability ?~ [a | a <- oldAvail, not $ elem a ts]
  where
    oldAvail = fromJust $ p ^. availability

updateFacultySchedule :: Map (Text, Text) [TimeSpan] -> [((Text, Text), [TimeSpan])] -> Map (Text, Text) [TimeSpan]
updateFacultySchedule sched assign = Map.fromList $ Map.toList sched ++ assign

updateFacultyGroupSchedule :: Map [(Text, Text)] [TimeSpan] -> [([(Text, Text)], [TimeSpan])] -> Map [(Text, Text)] [TimeSpan]
updateFacultyGroupSchedule sched assign = Map.fromList $ Map.toList sched ++ assign


makeContext :: [Person] -> [Person] -> [TimeSpan] -> Ctx
makeContext faculty prospects slots = (maxes, facLU, prospLU, slotLU)
  where
    nFac = length faculty
    nProsp = length prospects
    nSlot = length slots
    maxes = (nFac, nProsp, nSlot)
    facLU = Bimap.fromList (zip [f ^. fullName | f <- faculty] [1..])
    prospLU = Bimap.fromList (zip [p ^. fullName | p <- prospects] [1..])
    slotLU = Bimap.fromList (zip slots [1..])

testVars flu plu slu pst fst (f, p, s) = sl `elem` pls && sl `elem` fls
  where
    sl = slu Bimap.!> s
    pls = pst Map.! (plu Bimap.!> p)
    fls = fst Map.! (flu Bimap.!> f)

groupProblem :: Ctx -> [Person] -> [Person] -> Map TimeSpan [TimeSpan] -> Int -> State -> Goal
groupProblem ctx interviewers interviewees slots tier state = undefined
--  let mtgs = 
--  return ()


gms = [ (("Neha", "Verma"), [("Kenton", "Murray"), ("Kevin", "Duh")])
      , (("Orion", "Weller"), [("Paul", "Mcnamee"), ("Kevin", "Duh")])
      , (("Tucker", "Berckmann"), [("Kenton", "Murray"), ("Kevin", "Duh")])
      ]



-- faculty exact
-- faculty loose
-- cogsci students
-- non-cogsci students
-- groups
problem :: Ctx -> [Person] -> [Person] -> Map TimeSpan [TimeSpan] -> Int -> Map Int Int -> Bool -> State -> Goal
problem ctx faculty prospects slots tier rem strict state = do
  
  let ((nFac, nProsp, nSlot), flu, plu, slu) = ctx
      pst = Map.fromList $ [((p^.firstName, p^.lastName), fromJust $ p^.availability) | p <- prospects]
      fst = Map.fromList $ [((p^.firstName, p^.lastName), fromJust $ p^.availability) | p <- faculty]
      gms' = map (\(p, fs) -> (plu Bimap.! p, map (flu Bimap.!) fs)) gms
      ignore = concat $ map (\(p, fs) -> [(f, p) | f <- fs]) gms'
      
  -- individual meeting variables
  iVars' <- sequence $ [sBool $ show (i, j, k) | i <- [1..nFac], j <- [1..nProsp], k <- [1..nSlot], testVars flu plu slu pst fst (i, j, k)]

  -- variable lookups
  let iNames = [(i, j, k) | i <- [1..nFac], j <- [1..nProsp], k <- [1..nSlot], testVars flu plu slu pst fst (i, j, k)]

  let iVars = Map.fromList $ zip iNames iVars'
      maxes = (nFac, nProsp, nSlot)
      v = Map.size iVars


  -- fulfill individual meeting preferences
  sequence $ map (ensureIndividualMeeting ctx iVars state) [m | m <- fromMaybe [] (state^.requestedMeetings), (Set.size $ m^.interviewerParticipants) == 1]

  -- never double-book faculty (same slot, multiple prospects)
  sequence $ [constrain $ pbAtMost [iVars Map.! (f, p, s) | p <- [1..nProsp], (f,p,s) `Map.member` iVars] 1 | f <- [1..nFac], s <- [1..nSlot]]
  
  -- never double-book prospects (same slot, multiple faculty)
  let oneFac = [flu Bimap.! (traceShowId $ fac^.fullName) | fac <- faculty, (fac^.maxMeetingSize) == 1]
      twoFac = [flu Bimap.! (fac^.fullName) | fac <- faculty, (fac^.maxMeetingSize) == 2]

  -- never mix one and two meeting folks
  --sequence $ [constrain $ pbAtMost [iVars Map.! (f, p, s) |   
  --sequence $ [constrain $ pbAtMost [iVars Map.! (f, p, s) | f <- oneFac, (f,p,s) `Map.member` iVars] 1 | p <- [1..nProsp], s <- [1..nSlot]]
  --sequence $ [constrain $ pbAtMost [iVars Map.! (f, p, s) | f <- twoFac, (f,p,s) `Map.member` iVars] 2 | p <- [1..nProsp], s <- [1..nSlot]]
  -- keep faculty and others separate
  --sequence $ [constrain $ pbLe [(if f `elem` oneFac then 2 else 1, v) | ((f, p, s), v) <- Map.toList iVars, s == s', p == p'] 2| s' <- [1..nSlot], p' <- [1..nProsp]]

  -- at least one group must have zero attendance for each possible meeting
  --sequence $ [constrain $ pbAtLeast [ pbExactly [iVars Map.! (f, p, s) | f <- oneFac, (f, p, s) `Map.member` iVars] 0
  --                                  , pbExactly [iVars Map.! (f, p, s) | f <- twoFac, (f, p, s) `Map.member` iVars] 0
  --                                  ] 1 | s <- [1..nSlot], p <- [1..nProsp]]
  sequence $ [constrain $ pbExactly [pbExactly [iVars Map.! (f, p, s) | f <- fs, (f, p, s) `Map.member` iVars] (length fs) | s <- [1..nSlot]] 1 | (p, fs) <- gms']
  
  sequence $ [constrain $ pbExactly [ pbExactly [ pbExactly [iVars Map.! (f, p, s) | f <- oneFac, (f, p, s) `Map.member` iVars && (not $ (f, p) `elem` ignore)] 1
                                                , pbExactly [iVars Map.! (f, p, s) | f <- twoFac, (f, p, s) `Map.member` iVars && (not $ (f, p) `elem` ignore)] 0
                                                ] 2
                                    , pbExactly [ pbExactly [iVars Map.! (f, p, s) | f <- twoFac, (f, p, s) `Map.member` iVars && (not $ (f, p) `elem` ignore)] 2
                                                , pbExactly [iVars Map.! (f, p, s) | f <- oneFac, (f, p, s) `Map.member` iVars && (not $ (f, p) `elem` ignore)] 0
                                                ] 2
                                    , pbExactly [iVars Map.! (f, p, s) | f <- [1..nFac], (f, p, s) `Map.member` iVars && (not $ (f, p) `elem` ignore)] 0
                                    ] 1 | s <- [1..nSlot], p <- [1..nProsp]]    

  -- make sure no faculty-prospect pair meets more than once
  sequence $ [constrain $ pbAtMost ([v | ((f', p', _), v) <- Map.toList iVars, f' == f && p' == p]) 1 | f <- [1..nFac], p <- [1..nProsp]]

  -- don't reschedule if they already meet
  --sequence $ [constrain $ pbExactly ([v | ((f', p', _), v) <- Map.toList iVars, f' == f && p' == p]) 0 | f <- [1..nFac], p <- [1..nProsp],
  --            (plu Bimap.!> p) `Map.member` fromMaybe Map.empty ((flu Bimap.!> f) `Map.lookup` (fromJust $ state^.individualMeetings))
  --            ]

  -- require minimum number of meetings per faculty
  sequence $ [constrain $ pbAtLeast [b | ((f, p, s), b) <- Map.toList iVars, (flu Bimap.! (fac^.fullName)) == f] (fac^.minMeetings) | fac <- faculty]

  -- limit maximum number of meetings per faculty
  sequence $ [constrain $ pbAtMost [b | ((f, p, s), b) <- Map.toList iVars, (flu Bimap.! (fac^.fullName)) == f] (fac^.maxMeetings) | fac <- faculty]

  -- require minimum number of meetings per prospect
  sequence $ [constrain $ pbAtLeast [b | ((f, p, s), b) <- Map.toList iVars, (plu Bimap.! (pr^.fullName)) == p] (pr^.minMeetings) | pr <- prospects]

  -- limit maximum number of meetings per prospect
  sequence $ [constrain $ pbAtMost [b | ((f, p, s), b) <- Map.toList iVars, (plu Bimap.! (pr^.fullName)) == p] (pr^.maxMeetings) | pr <- prospects]
  
  --sequence $ [softConstrain $ pbAtLeast [v | ((_, p', _), v) <- Map.toList iVars, p' == p] n | (p, n) <- Map.toList rem]
  --sequence $ [constrain $ pbExactly [v | ((_, p', _), v) <- Map.toList iVars, p' == p] n | (p, n) <- Map.toList rem]

  -- limit the maximal number of meetings per prospect
  --sequence $ [constrain $ pbAtMost [v | ((_, p', _), v) <- Map.toList iVars, p' == p] (max n 2) | (p, n) <- Map.toList pmaxes]



  -- if tier == 1 then
  --   do
  --     sequence $ [constrain $ pbAtMost [v | ((f', _, _), v) <- Map.toList iVars, f' == f] 19 | f <- [1..nFac]] --(p, n) <- Map.toList (traceShowId rem)]
  --     sequence $ [constrain $ pbAtMost [v | ((f', _, _), v) <- Map.toList iVars, f' == (flu Bimap.! f)] n | (f, n) <- [
  --                    (("Jason", "Eisner"), 8),
  --                    --(("Ben", "Van Durme"), 19),
  --                    (("David", "Yarowsky"), 9),
  --                    (("Mark", "Dredze"), 9),
  --                    (("Tom", "Lippincott"), 10),
  --                    (("Philipp", "Koehn"), 12),
  --                    (("Ayah", "Zirikly"), 10),
  --                    (("Kevin", "Duh"), 11),
  --                    (("Dawn", "Lawrie"), 11),
  --                    (("Alan", "Yuille"), 1)
  --                    ]]
  --     sequence $ [constrain $ pbAtMost [v | ((f', _, _), v) <- Map.toList iVars, f' == (flu Bimap.! f)] n | (f, n) <- [        
  --                    (("Kenton", "Murray"), 6),
  --                    (("Patrick", "Xia"), 6),
  --                    (("Nils", "Holzenberger"), 6)
  --                    ]]  
  --     -- sequence $ [constrain $ pbAtMost [v | ((f', _, _), v) <- Map.toList iVars, f' == (flu Bimap.! f)] n | (f, n) <- [
  --     --                (("Jason", "Eisner"), 6),
  --     --                --(("Ben", "Van Durme"), 19),
  --     --                (("David", "Yarowsky"), 11),
  --     --                (("Mark", "Dredze"), 10),
  --     --                (("Tom", "Lippincott"), 11),
  --     --                (("Philipp", "Koehn"), 12),
  --     --                (("Ayah", "Zirikly"), 8),
  --     --                (("Kevin", "Duh"), 11),
  --     --                (("Dawn", "Lawrie"), 11)
  --     --                ]]
  --     -- sequence $ [constrain $ pbExactly [v | ((f', _, _), v) <- Map.toList iVars, f' == (flu Bimap.! f)] n | (f, n) <- [        
  --     --                (("Kenton", "Murray"), 6),
  --     --                (("Patrick", "Xia"), 6)
  --     --                ]]  
  --     --sequence $ [constrain $ pbAtMost [v | ((f', _, _), v) <- Map.toList iVars, f' == (flu Bimap.! f)] 10 | f <- [("Jason", "Eisner"), ("Patrick", "Xia")]]
  --   else return []
    
  return ()

-- preferenceConstraint :: Ctx -> Vars -> Int -> Map Int Int -> [Person] -> Bool -> State -> Person -> Goal
-- preferenceConstraint ctx@(maxes, flu, _, _) vars tier rem prosps strict state f = do
--   let (_, fname) = f ^. fullName
--       fid = flu Bimap.! (f ^. fullName)
--       mtgs = [m | m <- fromMaybe [] (state^.requestedMeetings), (f^.firstName, f^.lastName) `Set.member` (m^.interviewerParticipants)]
--       --mtgs = [head [x | x <- prosps, x ^. fullName == (p ^. fullName)] | (p, n) <- ((Map.toList . fromJust . _preferences) f), n == tier]
--       --pb = if Map.size rem == 0 || f^.lastName == "Yuille" then pbExactly else pbAtLeast
--       --pb = pbExactly
--   sequence $ map (ensureMeeting ctx vars f strict) mtgs
--   if tier == 2 then constrain $ pbExactly [v | ((f', _, _), v) <- Map.toList vars, f' == fid] (length mtgs) else return ()
--   return ()


ensureMeeting :: Ctx -> Vars -> Person -> Bool -> Person -> Goal
ensureMeeting (maxes, flu, plu, slu) vars fac strict prosp = do
  let fid = flu Bimap.! (fac ^. fullName)
      pid = plu Bimap.! (prosp ^. fullName)
      availF = (Set.fromList . fromJust) $ _availability fac
      availP = (Set.fromList . fromJust) $ _availability prosp
      avail = Set.toList $ Set.intersection availF availP
      sids = catMaybes $ map (\x -> Bimap.lookup x slu) avail
      prefs = ((fromJust . _preferences) fac)
      num = Map.findWithDefault (0) prosp prefs
  (if strict then constrain else softConstrain) $ pbExactly [vars Map.! (fid, pid, sid) | sid <- sids] 1 --(traceShowId num)
  constrain $ pbAtMost [vars Map.! (fid, pid, sid) | sid <- sids] 1


ensureIndividualMeeting :: Ctx -> Vars -> State -> Preference -> Goal
ensureIndividualMeeting (maxes, flu, plu, slu) vars state mtg = do
  let fac = (head . Set.toList) $ mtg^.interviewerParticipants
      prosp = (head . Set.toList) $ mtg^.intervieweeParticipants
      fid = flu Bimap.! fac --(fac ^. fullName)
      pid = plu Bimap.! prosp -- (prosp ^. fullName)
      fac' = head $ [x | x <- state^.faculty, (x^.firstName == (fst fac)) && (x ^.lastName == (snd fac))]
      prosp' = head $ [x | x <- state^.prospects, (x^.firstName == (fst prosp)) && (x ^.lastName == (snd prosp))]
      availF = (Set.fromList . fromJust) $ _availability fac'
      availP = (Set.fromList . fromJust) $ _availability prosp'
      avail = Set.toList $ Set.intersection availF availP
      sids = catMaybes $ map (\x -> Bimap.lookup x slu) avail
      strict = mtg^.required
  (if strict then constrain else softConstrain) $ pbExactly [vars Map.! (fid, pid, sid) | sid <- sids] 1 --(traceShowId num)
  constrain $ pbAtMost [vars Map.! (fid, pid, sid) | sid <- sids] 1  
  return ()

ensureGroupMeeting :: Ctx -> Vars -> State -> Preference -> Goal
ensureGroupMeeting (maxes, flu, plu, slu) vars state mtg = do
  let facs = Set.toList $ mtg^.interviewerParticipants
      prosp = (head . Set.toList) $ mtg^.intervieweeParticipants
      fids = map (flu Bimap.!) facs --(fac ^. fullName)
      pid = plu Bimap.! prosp -- (prosp ^. fullName)
      facs' = map (\(f, l) -> head $ [x | x <- state^.faculty, (x^.firstName == f) && (x ^.lastName == l)]) facs
      prosp' = head $ [x | x <- state^.prospects, (x^.firstName == (fst prosp)) && (x ^.lastName == (snd prosp))]
      --availF = (Set.fromList . fromJust) $ _availability fac
      --availP = (Set.fromList . fromJust) $ _availability prosp
      --avail = Set.toList $ Set.intersection availF availP
  --     sids = catMaybes $ map (\x -> Bimap.lookup x slu) avail
  --     prefs = ((fromJust . _preferences) fac)
  --     num = Map.findWithDefault (0) prosp prefs
  -- (if strict then constrain else softConstrain) $ pbExactly [vars Map.! (fid, pid, sid) | sid <- sids] 1 --(traceShowId num)
  -- constrain $ pbAtMost [vars Map.! (fid, pid, sid) | sid <- sids] 1
  return ()
-- preferenceConstraint' :: Ctx -> Vars -> Int -> [Person] -> Person -> Goal
-- preferenceConstraint' ctx@(maxes, _, _, _) vars tier prosps f = do
--   sequence $ map (ensureMeeting' ctx vars f tier) [head [x | x <- prosps, x ^. fullName == (p ^. fullName)] | (p, n) <- ((Map.toList . fromJust . _preferences) f), n == tier]
--   return ()


-- ensureMeeting' :: Ctx -> Vars -> Person -> Int -> Person -> Goal
-- ensureMeeting' (maxes, flu, plu, slu) vars fac tier prosp = do
--   let fid = flu Bimap.! (fac ^. fullName)
--       pid = plu Bimap.! (prosp ^. fullName)
--       availF = (Set.fromList . fromJust) $ _availability fac
--       availP = (Set.fromList . fromJust) $ _availability prosp
--       avail = Set.toList $ Set.intersection availF availP
--       sids = catMaybes $ map (\x -> Bimap.lookup x slu) avail
--       prefs = ((fromJust . _preferences) fac)
--       num = Map.findWithDefault (-1) prosp prefs
--   -- liftIO $ print (fid, pid, availF)
--   -- liftIO $ print (fid, pid, availP)
--   -- liftIO $ print (fid, pid, avail)
--   -- liftIO $ print "\n"
--   softConstrain $ pbExactly [vars Map.! (fid, pid, sid) | sid <- sids] 1
--   constrain $ pbAtMost [vars Map.! (fid, pid, sid) | sid <- sids] 1


printSchedules :: State -> IO ()
printSchedules state = do
  let facs = _faculty state
      prosps = _prospects state
      sts = ((fromJust . _slots) state)
      ctx = makeContext facs prosps sts
      ((nFac, nProsp, nSlot), fLU, pLU, sLU) = ctx
      sched = fromMaybe Map.empty (state ^. individualMeetings)
      vars = Map.fromList [((f, p, s), elem (sLU Bimap.!> s) (fromMaybe [] $ (fromMaybe Map.empty $ sched Map.!? ((fLU Bimap.!> f))) Map.!? (pLU Bimap.!> p))) | f <- [1..nFac], p <- [1..nProsp], s <- [1..nSlot]]
  let facGrid = [[printf "%2d-%14v" i (snd $ fLU Bimap.!> f)] ++ [case (head $ [p | p <- [1..nProsp], vars Map.! (f, p, s) == True] ++ [0] :: Int) of 0 -> "  "; v -> printf "%2d" v | s <- [1..nSlot]] | (i, f) <- zip [1 :: Int ..] [1..nFac]] :: [[String]]
      prospGrid = [[printf "%2d-%14v" i (snd $ pLU Bimap.!> p)] ++ [case (head $ [f | f <- [1..nFac], vars Map.! (f, p, s) == True] ++ [0] :: Int) of 0 -> "  "; v -> printf "%2d" v | s <- [1..nSlot]] | (i, p) <- zip [1 :: Int ..] [1..nProsp]] :: [[String]]      
  putStrLn $ unlines (map unwords facGrid)
  putStrLn $ unlines (map unwords prospGrid)
