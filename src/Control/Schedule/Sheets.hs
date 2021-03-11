{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RankNTypes #-}

module Control.Schedule.Sheets (readSpreadsheet, readState, writeForms, writeSchedule, sheetLookup) where

import Prelude hiding (group)
import qualified Network.Google.Sheets as S
import Network.Google.Resource.Sheets.Spreadsheets.Get
import Network.Google.Sheets hiding (Text, sheet)
import Network.Google
import Data.Aeson.Types
import Data.Text (Text, unpack, pack)
import qualified Data.Text as T
import Text.Printf (printf, PrintfArg(..), fmtPrecision, fmtChar, errorBadFormat, formatString, vFmt, IsChar)
import Control.Lens hiding (zoom)
import Control.Exception (try, tryJust)
import Control.Monad (join)
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Time.Clock (UTCTime(..), secondsToNominalDiffTime)
import Data.Time.Format
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random
import Data.Int (Int32)
import Control.Schedule.State
import Control.Schedule.Person
import Control.Schedule.TimeSpan
import Control.Schedule.Preference
import Data.Time
import Text.Read (readMaybe)
import Data.List (intercalate, sort)
import Debug.Trace (traceShowId)
import qualified Data.Text as Text


readState :: Maybe [TimeSpan] -> Spreadsheet -> State
readState allSlots ssheet = simpleState & faculty .~ faculty' & prospects .~ prospects' & slots .~ allSlots & individualMeetings .~ indiv & requestedMeetings .~ requestedMeetings'
  where
    prospects' = (getAvailability "Prospect Availability" ssheet . getPeople "Prospects") ssheet
    faculty' = (getAvailability "Interviewer Availability" ssheet . getPeople "Interviewers") ssheet
    requestedMeetings' = Just $ (getIndividualPrefs "Individual Preferences" ssheet) ++ (getGroupPrefs "Group Preferences" ssheet)
    slu = ssheet^.sheetLookup    
    flu = Map.fromList $ [(T.unwords [T.take 1 (p ^. firstName), p ^. lastName], (p ^. firstName, p ^. lastName)) | p <- prospects']
    indiv = Just $ if "Interviewer Schedule" `Map.member` slu then Map.fromList $ [((f ^. firstName, f ^. lastName), getFacultySchedule "Interviewer Schedule" ssheet f flu) | f <- faculty'] else Map.empty


writeForms :: Text -> Maybe Int32 -> Maybe Int32 -> State -> IO ()
writeForms sid aid pid state = do
  case pid of
    Nothing -> return ()
    Just tid -> deleteTab sid tid
  case aid of
    Nothing -> return ()
    Just tid -> deleteTab sid tid    
  createPreferenceForm sid (state ^. faculty) (state ^. prospects)
  createAvailabilityForm sid (state ^. faculty) (fromJust $ state ^. slots)
  return ()


deleteTab :: Text -> Int32 -> IO ()
deleteTab sid tid = do
  env <- newEnv <&> (envScopes .~ spreadsheetsScope)
  let req = deleteSheetRequest & dsrSheetId .~ Just tid
  resp <- try . runResourceT . runGoogle env $ send (spreadsheetsBatchUpdate sid (busrRequests .~ [(reqDeleteSheet .~ Just req $ request')] $ batchUpdateSpreadsheetRequest)) :: IO (Either Error BatchUpdateSpreadsheetResponse)
  return ()


preferenceEntryFormat :: Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> Int -> [ConditionalFormatRule]
preferenceEntryFormat tid sr sc er ec n = [neg]
  where
    cv = [conditionValue & cvUserEnteredValue ?~ (pack . show) i | i <- [0, n]]
    bc = booleanCondition & bcValues .~ cv & bcType ?~ NumberNotBetween
    cf = cellFormat & cfBackgRoundColor ?~ (color & cRed ?~ 0.9 & cAlpha ?~ 0.1)
    br = booleanRule & brFormat ?~ cf & brCondition ?~ bc
    range = gridRange & grSheetId ?~ tid & grStartRowIndex ?~ sr & grEndRowIndex ?~ (fromIntegral $ er + 1) & grStartColumnIndex ?~ sc & grEndColumnIndex ?~ (fromIntegral $ ec + 1)
    neg = conditionalFormatRule & cfrBooleanRule ?~ br & cfrRanges .~ [range]


availabilityEntryFormat :: Int32 -> Int32 -> Int32 -> Int32 -> Int32 -> [ConditionalFormatRule]
availabilityEntryFormat tid sr sc er ec = [neg]
  where
    bc = booleanCondition & bcType ?~ NotBlank
    cf = cellFormat & cfBackgRoundColor ?~ (color & cGreen ?~ 0.9 & cAlpha ?~ 0.1)
    br = booleanRule & brFormat ?~ cf & brCondition ?~ bc
    range = gridRange & grSheetId ?~ tid & grStartRowIndex ?~ sr & grEndRowIndex ?~ (fromIntegral $ er + 1) & grStartColumnIndex ?~ sc & grEndColumnIndex ?~ (fromIntegral $ ec + 1)
    neg = conditionalFormatRule & cfrBooleanRule ?~ br & cfrRanges .~ [range]


headerFormat :: TextFormatRun
headerFormat = textFormatRun & tfrStartIndex .~ Just 0 & tfrFormat .~ Just (textFormat & tfBold .~ Just True)


--prefHelp = extendedValue & evStringValue ?~ "Instructions: indicate how much time you would like with each prospect, from 0-4 15-minute increments.  0 indicates you *will not* meet with the student, while a blank indicates you are *willing* to do so, to help round out the schedule.  The form will highlight invalid entries in red."
prefHelp = extendedValue & evStringValue ?~ "Instructions: enter '1' if you consider the student a potential advisee (one-on-one, hour-long meeting), '0' if you won't meet with the student *at all* (use sparingly, if at all), blank indicates you will meet with the student as part of a group" -- , but don't consider them a match as an advisee."

schedHelp = extendedValue & evStringValue ?~ "Instructions: enter any value to indicate you are available at that time (the particular value doesn't matter)"

--"how much time you would like with each prospect, from 0-4 15-minute increments.  0 indicates you *will not* meet with the student, while a blank indicates you are *willing* to do so, to help round out the schedule.  The form will highlight invalid entries in red."


createPreferenceForm sid facs prosps = do
  env <- newEnv <&> (envScopes .~ spreadsheetsScope)
  let nRows = fromIntegral $ length prosps + 2
      nCols = fromIntegral $ length facs + 2
  tid <- addTab sid "Interviewer Preferences" nRows nCols 3
  let range = gridRange & grStartRowIndex .~ Just 0 & grEndRowIndex .~ Just nRows & grStartColumnIndex .~ Just 0 & grEndColumnIndex .~ Just nCols & grSheetId .~ Just tid
      header = map (\x -> x & cdTextFormatRuns .~ [headerFormat]) ([cellData & cdUserEnteredValue ?~ (extendedValue & evStringValue ?~ "First"), cellData & cdUserEnteredValue ?~ (extendedValue & evStringValue ?~ "Last")] ++ [cellData & cdUserEnteredValue .~ Just (extendedValue & evStringValue .~ Just _lastName) | Person{..} <- facs])
      rows = [rowData & rdValues .~ header] ++ [preferenceRow i (fromIntegral nCols) p | (i, p) <- zip [1..] prosps]
      help = [rowData & rdValues .~ [cellData & cdUserEnteredValue ?~ prefHelp]]
      req = updateCellsRequest & updRange .~ Just range & updFields .~ (Just . GFieldMask) "*"  & updRows .~ rows
      req' = updateCellsRequest & updRange ?~ (gridRange & grStartRowIndex ?~ (nRows - 1) & grEndRowIndex ?~ nRows & grStartColumnIndex ?~ 0 & grEndColumnIndex ?~ 1 & grSheetId ?~ tid) & updRows .~ help & updFields ?~ GFieldMask "*"
      fmts = [addConditionalFormatRuleRequest & acfrrRule ?~ fmt | fmt <- preferenceEntryFormat tid 1 2 (nRows - 1) nCols 4]
      fmtReqs = [reqAddConditionalFormatRule ?~ fmtReq $ request' | fmtReq <- fmts]
      resize = autoResizeDimensionsRequest & ardrDimensions ?~ (dimensionRange & drDimension ?~ DRDColumns & drSheetId ?~ tid & drStartIndex ?~ 0 & drEndIndex ?~ nCols)
  try . runResourceT . runGoogle env $ send (spreadsheetsBatchUpdate sid (busrRequests .~ [(reqUpdateCells .~ Just req $ request')] ++ fmtReqs ++ [reqAutoResizeDimensions .~ Just resize $ request']$ batchUpdateSpreadsheetRequest)) :: IO (Either Error BatchUpdateSpreadsheetResponse)
  try . runResourceT . runGoogle env $ send (spreadsheetsBatchUpdate sid (busrRequests .~ [reqAutoResizeDimensions .~ Just resize $ request']$ batchUpdateSpreadsheetRequest)) :: IO (Either Error BatchUpdateSpreadsheetResponse)
  try . runResourceT . runGoogle env $ send (spreadsheetsBatchUpdate sid (busrRequests .~ [(reqUpdateCells .~ Just req' $ request')] $ batchUpdateSpreadsheetRequest)) :: IO (Either Error BatchUpdateSpreadsheetResponse)
  return ()


preferenceRow :: Int -> Int -> Person -> RowData
preferenceRow row nCols prosp = undefined --rowData & rdValues .~ (filled ++ rest)
  where
    f = prosp ^. firstName
    l = prosp ^. lastName
    --b = prosp ^. biography
    --u = fromMaybe "www.google.com" (prosp ^. application)
    --f' = T.concat ["=HYPERLINK(\"", u, "\", \"", f, "\")"]
    --l' = T.concat ["=HYPERLINK(\"", u, "\", \"", l, "\")"]
    --fv = extendedValue & evFormulaValue .~ Just f'    
    --lv = extendedValue & evFormulaValue .~ Just l'
    --filled = [cellData & cdUserEnteredValue .~ Just fv & cdNote .~ b, cellData & cdUserEnteredValue .~ Just lv & cdNote .~ b]
    --rest = replicate (nCols - (length filled)) (cellData & cdNote ?~ "")


availabilityRow :: Int -> Int -> TimeSpan -> RowData
availabilityRow row nCols slot = rowData & rdValues .~ (filled ++ rest)
  where
    f = slot ^. start
    l = slot ^. end
    f' = (pack . formatTime defaultTimeLocale "%a %R") f
    l' = (pack . formatTime defaultTimeLocale "%a %R") l
    fv = extendedValue & evStringValue .~ Just f'    
    lv = extendedValue & evStringValue .~ Just l'
    filled = [cellData & cdUserEnteredValue .~ Just fv, cellData & cdUserEnteredValue .~ Just lv]
    rest = replicate (nCols - (length filled)) cellData


writeGrid sid tid xs = do
  undefined


createAvailabilityForm sid facs slots = do
  env <- newEnv <&> (envScopes .~ spreadsheetsScope)
  let nRows = fromIntegral $ length slots + 2
      nCols = fromIntegral $ length facs + 2
  tid <- addTab sid "Interviewer Availability" nRows nCols 3
  let range = gridRange & grStartRowIndex .~ Just 0 & grEndRowIndex .~ Just nRows & grStartColumnIndex .~ Just 0 & grEndColumnIndex .~ Just nCols & grSheetId .~ Just tid
      header = map (\x -> x & cdTextFormatRuns .~ [headerFormat]) ([cellData & cdUserEnteredValue ?~ (extendedValue & evStringValue ?~ "Start"), cellData & cdUserEnteredValue ?~ (extendedValue & evStringValue ?~ "End")] ++ [cellData & cdUserEnteredValue .~ Just (extendedValue & evStringValue .~ Just _lastName) | Person{..} <- facs])      
      rows = [rowData & rdValues .~ header] ++ [availabilityRow i (fromIntegral nCols) p | (i, p) <- zip [1..] slots]
      help = [rowData & rdValues .~ [cellData & cdUserEnteredValue ?~ schedHelp]]
      req = updateCellsRequest & updRange .~ Just range & updFields .~ (Just . GFieldMask) "*"  & updRows .~ rows
      req' = updateCellsRequest & updRange ?~ (gridRange & grStartRowIndex ?~ (nRows - 1) & grEndRowIndex ?~ nRows & grStartColumnIndex ?~ 0 & grEndColumnIndex ?~ 1 & grSheetId ?~ tid) & updRows .~ help & updFields ?~ GFieldMask "*"
      fmts = [addConditionalFormatRuleRequest & acfrrRule ?~ fmt | fmt <- availabilityEntryFormat tid 1 2 nRows nCols]
      fmtReqs = [reqAddConditionalFormatRule ?~ fmtReq $ request' | fmtReq <- fmts]
      resize = autoResizeDimensionsRequest & ardrDimensions ?~ (dimensionRange & drDimension ?~ DRDColumns & drSheetId ?~ tid & drStartIndex ?~ 0 & drEndIndex ?~ nCols)
  try . runResourceT . runGoogle env $ send (spreadsheetsBatchUpdate sid (busrRequests .~ [(reqUpdateCells .~ Just req $ request')] ++ fmtReqs ++ [reqAutoResizeDimensions .~ Just resize $ request']$ batchUpdateSpreadsheetRequest)) :: IO (Either Error BatchUpdateSpreadsheetResponse)
  try . runResourceT . runGoogle env $ send (spreadsheetsBatchUpdate sid (busrRequests .~ [(reqUpdateCells .~ Just req' $ request')] $ batchUpdateSpreadsheetRequest)) :: IO (Either Error BatchUpdateSpreadsheetResponse)
  return ()


writeSchedule :: Text -> Maybe Int32 -> Maybe Int32 -> State -> [TimeSpan] -> IO ()
writeSchedule sid tidF tidP state ts = do

  let ims = fromMaybe Map.empty (state ^. individualMeetings)
      --gms = fromMaybe Map.empty (state ^. groupMeetings)
  case tidF of
    Nothing -> return ()
    Just tid -> deleteTab sid tid
  let nRows = fromIntegral $ 1 + (length $ state ^. faculty)
      nCols = fromIntegral $ 2 + (length ts)
  env <- newEnv <&> (envScopes .~ spreadsheetsScope)
  tidF' <- addTab sid "Interviewer Schedule" nRows nCols 3
  let range = gridRange & grStartRowIndex .~ Just 0 & grEndRowIndex .~ Just nRows & grStartColumnIndex .~ Just 0 & grEndColumnIndex .~ Just nCols & grSheetId .~ Just tidF'
      header = map (\x -> x & cdTextFormatRuns .~ [headerFormat]) ([cellData & cdUserEnteredValue ?~ (extendedValue & evStringValue ?~ "First"), cellData & cdUserEnteredValue ?~ (extendedValue & evStringValue ?~ "Last")] ++ [cellData & cdUserEnteredValue .~ Just (extendedValue & evStringValue .~ Just (pack . formatTime defaultTimeLocale "%a %R" $ t ^. start)) | t <- ts])
      rows = [rowData & rdValues .~ header] ++ [scheduleFacultyRow ts n v | (n, v) <- (Map.toList . fromJust) (state ^. individualMeetings)]
      fmtReqs = []
      req = updateCellsRequest & updRange .~ Just range & updFields .~ (Just . GFieldMask) "*"  & updRows .~ rows
  try . runResourceT . runGoogle env $ send (spreadsheetsBatchUpdate sid (busrRequests .~ [(reqUpdateCells .~ Just req $ request')] ++ fmtReqs $ batchUpdateSpreadsheetRequest)) :: IO (Either Error BatchUpdateSpreadsheetResponse)

  case tidP of
    Nothing -> return ()
    Just tid -> deleteTab sid tid    

  let nRows = fromIntegral $ 1 + (length $ state ^. prospects)
      nCols = fromIntegral $ 2 + (length ts)
  env <- newEnv <&> (envScopes .~ spreadsheetsScope)
  tidP' <- addTab sid "Prospect Schedule" nRows nCols 2
  let range = gridRange & grStartRowIndex .~ Just 0 & grEndRowIndex .~ Just nRows & grStartColumnIndex .~ Just 0 & grEndColumnIndex .~ Just nCols & grSheetId .~ Just tidP'
      header = map (\x -> x & cdTextFormatRuns .~ [headerFormat]) ([cellData & cdUserEnteredValue ?~ (extendedValue & evStringValue ?~ "First"), cellData & cdUserEnteredValue ?~ (extendedValue & evStringValue ?~ "Last")] ++ [cellData & cdUserEnteredValue .~ Just (extendedValue & evStringValue .~ Just (pack . formatTime defaultTimeLocale "%a %R" $ t ^. start)) | t <- ts])
      rows = [rowData & rdValues .~ header] ++ [scheduleProspectRow ts (p ^. fullName) (fromMaybe Map.empty $ state ^. individualMeetings)| p <- (state ^. prospects)]
      fmtReqs = []
      req = updateCellsRequest & updRange .~ Just range & updFields .~ (Just . GFieldMask) "*"  & updRows .~ rows
  x <- try . runResourceT . runGoogle env $ send (spreadsheetsBatchUpdate sid (busrRequests .~ [(reqUpdateCells .~ Just req $ request')] ++ fmtReqs $ batchUpdateSpreadsheetRequest)) :: IO (Either Error BatchUpdateSpreadsheetResponse)
  --print x
  return ()


scheduleFacultyRow :: [TimeSpan] -> (Text, Text) -> Map (Text, Text) [TimeSpan] -> RowData
scheduleFacultyRow ts (first, last) vs = rowData & rdValues .~ ([first', last'] ++ rest')
  where
    fv = extendedValue & evStringValue .~ Just first
    lv = extendedValue & evStringValue .~ Just last
    first' = cellData & cdUserEnteredValue .~ Just fv
    last' = cellData & cdUserEnteredValue .~ Just lv
    restMap = Map.fromList $ concat [[(x, pack $ [head $ unpack f] ++ " " ++ (unpack l)) | x <- xs] | ((f, l), xs) <- Map.toList vs]
    --gRestMap = Map.fromList $ concat [[(x, T.intercalate "/" (map (id . snd) ns)) | x <- xs] | (ns, xs) <- Map.toList gvs]
    --rest = [extendedValue & evStringValue ?~ (Map.findWithDefault (if elem t knownEvents then "EVENT" else (Map.findWithDefault "" t gRestMap)) t restMap) | t <- ts]
    rest = [extendedValue & evStringValue ?~ (Map.findWithDefault "" t restMap) | t <- ts]
    rest' = [cellData & cdUserEnteredValue ?~ v | v <- rest]


scheduleProspectRow :: [TimeSpan] -> (Text, Text) -> Map (Text, Text) (Map (Text, Text) [TimeSpan]) -> RowData
scheduleProspectRow ts (first, last) vs = rowData & rdValues .~ ([first', last'] ++ rest')
  where
    fv = extendedValue & evStringValue .~ Just first
    lv = extendedValue & evStringValue .~ Just last
    first' = cellData & cdUserEnteredValue .~ Just fv
    last' = cellData & cdUserEnteredValue .~ Just lv

    rel = concat [[(ss, Text.intercalate " " [Text.take 1 $ fst f, snd f]) | (p, ss) <- Map.toList ps, p == (first, last)] | (f, ps) <- Map.toList vs]
    --rel' = (Map.fromList . map (\(a, b) -> (b, a)) . Map.toList) $
    rel' = Map.fromListWith (\a b -> Text.intercalate ", " [a, b]) rel
    restMap = Map.fromList $ concat [[(t, s) | t <- ts] | (ts, s) <- Map.toList rel']
    --vs''' = Map.fromListWith (\a b -> a) [(v, k) | (k, v) <- Map.toList $ Map.mapKeys (\(a, b) -> Text.intercalate " " [a,b]) vs]
    --restMap = Map.fromList $ concat [ concat [[(t, fs) | t <- vs''] | ((pf, pl), vs'') <- Map.toList vs', (pf, pl) == (first, last)] | (fs, vs') <- Map.toList vs''']
    --restMap = Map.fromList $ concat [ concat [[(t, pack $ [head $ unpack ff] ++ " " ++ (unpack fl)) | t <- vs''] | ((pf, pl), vs'') <- Map.toList vs', (pf, pl) == (first, last)] | ((ff, fl), vs') <- Map.toList vs]
    --gRestMap = Map.fromList $ concat [ concat [[(t, pack $ [head $ unpack ff] ++ " " ++ (unpack fl)) | t <- vs''] | (pns, vs'') <- Map.toList vs', (first, last) `elem` pns] | ((ff, fl), vs') <- Map.toList gvs]
    --gRestMap = Map.fromList []
  --   restMap = Map.fromList $ concat [[(x, pack $ [head $ unpack f] ++ " " ++ (unpack l)) | x <- xs] | ((f, l), xs) <- Map.toList vs]
    rest = [extendedValue & evStringValue ?~ (Map.findWithDefault "" t restMap) | t <- ts]
    rest' = [cellData & cdUserEnteredValue ?~ v | v <- rest]


  
-- createSchedule sid facs prosps slots = do
--   env <- newEnv <&> (envScopes .~ spreadsheetsScope)  
--   addTab sid "Schedule" (fromIntegral $ length prosps + 1) (fromIntegral $ length facs + 2)
--   let cols = ['A'..'Z']
--       facRange = pack $ printf "'Schedule'!A1:%v1" (cols !! (length facs + 2))
--       slotRange = pack $ printf "'Schedule'!A2:B%d" (length slots + 1)
--       facVals = ( vrMajorDimension .~ Just VRMDRows $ vrValues .~ [[String "Start", String "End"] ++ [String _lastName | Person{..} <- facs]] $ vrRange .~ Just facRange $ valueRange)
--       slotVals = ( vrMajorDimension .~ Just VRMDRows $ vrValues .~ [[String $ (pack . formatTime defaultTimeLocale "%a %R") (_start s), String $ (pack . formatTime defaultTimeLocale "%a %R") (_end s)] | s <- slots] $ vrRange .~ Just slotRange $ valueRange)      
--   runResourceT . runGoogle env $ send (svuValueInputOption .~ Just "USER_ENTERED" $ spreadsheetsValuesUpdate sid facVals facRange)
--   runResourceT . runGoogle env $ send (svuValueInputOption .~ Just "USER_ENTERED" $ spreadsheetsValuesUpdate sid slotVals slotRange)  


readSpreadsheet sid = do
  env <- newEnv <&> (envScopes .~ spreadsheetsScope)
  runResourceT . runGoogle env $ send (sgIncludeGridData .~ Just True $ spreadsheetsGet sid)


--readValues sid spec = do
--  env <- newEnv <&> (envScopes .~ spreadsheetsScope)
--  runResourceT . runGoogle env $ send (spreadsheetsValuesGet sid spec)


-- writeValues sid sheet startRow startColumn endRow endColumn values = do
--   env <- newEnv <&> (envScopes .~ spreadsheetsScope)
--   let cols = ['A'..'Z']  
--   return ()


addTab sid name nRows nCols index = do
  env <- newEnv <&> (envScopes .~ spreadsheetsScope)
  let gp = gridProperties & gpColumnCount ?~ nCols & gpRowCount ?~ nRows & gpFrozenRowCount ?~ 1 & gpFrozenColumnCount ?~ 2
      sp = sheetProperties & sTitle ?~ name & sGridProperties ?~ gp & sIndex ?~ (fromIntegral index)
      asr = asrProperties ?~ sp $ addSheetRequest
  rv' <- try . runResourceT . runGoogle env $ send (spreadsheetsBatchUpdate sid (busrRequests .~ [(reqAddSheet .~ Just asr $ request')] $ batchUpdateSpreadsheetRequest)) :: IO (Either Error BatchUpdateSpreadsheetResponse)
  --print (rv', asr)
  let Right rv = rv'
  return $ fromJust $ (fromJust $ (fromJust $ (head $ rv ^. busrReplies) ^. rAddSheet) ^. aProperties) ^. sSheetId


sheetName :: Sheet -> Maybe Text
sheetName s = join $ s ^. sProperties <&> view sTitle


sheetId :: Sheet -> Maybe Int32
sheetId s = join $ s ^. sProperties <&> view sSheetId
  

rowToPerson :: [Text] -> RowData -> Maybe Person
rowToPerson cols rd = do
  let cells = rd ^. rdValues  
      cells' = map (\x -> (x ^. cdFormattedValue, x^.cdHyperlink)) cells
      feats = Map.fromList $ zip cols cells'
      first = (fromJust . fst) $ feats Map.! "First"
      last = (fromJust .fst) $ feats Map.! "Last"
      minMtg = (read . unpack) $ fromMaybe "3" (fst $ feats Map.! "Minimum meetings") :: Int
      maxMtg = (read . unpack) $ fromMaybe "3" (fst $ feats Map.! "Maximum meetings") :: Int
      maxSz = (read . unpack) $ fromMaybe "2" (fst $ feats Map.! "Maximum size") :: Int
      reqOnly = (fromMaybe "FALSE" (fst $ feats Map.! "Requested only")) == "TRUE"
      e = (fst . fromJust) $ "Email" `Map.lookup` feats
      z = (fst . fromJust) $ "Zoom" `Map.lookup` feats
      g = (fst . fromJust) $ "Group" `Map.lookup` feats
      a = (snd . fromMaybe (Nothing, Just "")) $ "App" `Map.lookup` feats
  return $ simplePerson first last & email .~ e & zoom .~ z & minMeetings .~ minMtg & maxMeetings .~ maxMtg & requestedOnly .~ reqOnly & maxMeetingSize .~ maxSz & group .~ g & application .~ a


rowToSlot :: RowData -> Maybe TimeSpan
rowToSlot rd = do
  let time:_ = rd ^. rdValues
  time' <- time ^. cdFormattedValue
  let startTime = parseTimeOrError True defaultTimeLocale "" (unpack time')      
  return $ TimeSpan startTime startTime


tab :: Text -> Lens' Spreadsheet Sheet
tab name = lens (\ss -> fromJust $ ssheet ss name) undefined


ssheet :: Spreadsheet -> Text -> Maybe Sheet
ssheet ssheet name = case ss of
                      [] -> Nothing
                      s:_ -> Just s
  where
    ss = [s | s <- ssheet ^. sprSheets, sheetName s == Just name]


sheetLookup :: Lens' Spreadsheet (Map Text Int32)
sheetLookup = lens getter (\x y -> undefined)
  where
    getter ss = Map.fromList $ map (\s -> ((fromJust . sheetName) s, (fromJust . sheetId) s)) (ss ^. sprSheets)


rowToGroupPreference row = simplePreference & intervieweeParticipants .~ (Set.fromList [prosp]) & interviewerParticipants .~ (Set.fromList ints) & priority .~ 2
  where
    first:last:interviewers = rowToStrings row
    prosp = (first, last)
    ints = map ((\(a, b) -> (a, Text.strip b)) . Text.breakOn " ") (filter (\x -> x /= "") interviewers)
    

getGroupPrefs :: Text -> Spreadsheet -> [Preference]
getGroupPrefs sn ss = map rowToGroupPreference rows
  where
    (sheet:_) = [s | s <- ss ^. sprSheets, sheetName s == Just sn]
    (ds:_) = sheet ^. sData
    rows = ds ^. gdRowData . _tail


rowToIndividualPreferences cols row = catMaybes $ map (cellToIndividualPreference (first, last)) (zip cols entries)
  where
    _:_:first:last:entries = rowToStrings row

    
cellToIndividualPreference interviewee (interviewer, value) = case value of "" -> Nothing
                                                                            "x" -> Just $ simplePreference & intervieweeParticipants .~ (Set.fromList [interviewee]) & interviewerParticipants .~ (Set.fromList [interviewer]) & priority .~ 1 & required .~ True
                                                                            _ -> Just $ simplePreference & intervieweeParticipants .~ (Set.fromList [interviewee]) & interviewerParticipants .~ (Set.fromList [interviewer]) & priority .~ 0 & required .~ False


getIndividualPrefs :: Text -> Spreadsheet -> [Preference]
getIndividualPrefs sn ss = concat (map (rowToIndividualPreferences cols) rows)
  where
    (sheet:_) = [s | s <- ss ^. sprSheets, sheetName s == Just sn]
    (ds:_) = sheet ^. sData
    firsts = (drop 4 . rowToStrings . head) $ ds ^. gdRowData
    lasts = (drop 4 . rowToStrings . head . drop 1) $ ds ^. gdRowData
    cols = zip firsts lasts
    rows = (drop 2) $ ds ^. gdRowData


getPeople :: Text -> Spreadsheet -> [Person]
getPeople sn ss = catMaybes $ map (rowToPerson cols) rows
  where
    (sheet:_) = [s | s <- ss ^. sprSheets, sheetName s == Just sn]
    (ds:_) = sheet ^. sData
    cols = rowToStrings $ head $ ds ^. gdRowData
    rows = ds ^. gdRowData . _tail


rowToStrings :: RowData -> [Text]
rowToStrings r = map (\x -> fromMaybe "" (x ^. cdFormattedValue)) (r ^. rdValues)


mon = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "2021-03-08" :: Day
tue = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "2021-03-09" :: Day
wed = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "2021-03-10" :: Day
thu = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "2021-03-11" :: Day
fri = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "2021-03-12" :: Day
--sat = parseTimeOrError True defaultTimeLocale "%Y-%-m-%-d" "2021-02-06" :: Day


stringsToSlot :: Text -> Text -> TimeSpan
stringsToSlot s e = TimeSpan start end
  where
    sd:st:[] = map T.unpack (T.splitOn " " s)
    ed:et:[] = map T.unpack (T.splitOn " " e)
    d = case sd of "Mon" -> mon
                   "Tue" -> tue
                   "Wed" -> wed
                   "Thu" -> thu
                   "Fri" -> fri
                   --"Sat" -> sat
    start = UTCTime d (timeOfDayToTime (parseTimeOrError True defaultTimeLocale "%R" st :: TimeOfDay))
    end = UTCTime d (timeOfDayToTime (parseTimeOrError True defaultTimeLocale "%R" et :: TimeOfDay))


getFacultySchedule :: Text -> Spreadsheet -> Person -> Map Text (Text, Text) -> Map (Text, Text) [TimeSpan]
getFacultySchedule tabName ss fac plu = Map.map fixTimes scheds
  where
    rows = [r | r <- map rowToStrings $ (head  $ ss ^. tab tabName . sData) ^. gdRowData]
    (_:_:slots) = head rows
    row = drop 2 $ head [r | r@(f:l:_) <- rows, (f, l) == (fac^.firstName, fac^.lastName)]
    scheds = Map.fromListWith (++) [(plu Map.! n, [ts]) | (n, ts) <- zip row slots, n /= ""]


fixTimes times = times'
  where
    times' = sort $ map fixTime times
    --s = minimum times'
    --e = maximum times'

fixTime time = TimeSpan s e
  where
    sd = Text.take 3 time
    d = case sd of "Mon" -> mon
                   "Tue" -> tue
                   "Wed" -> wed
                   "Thu" -> thu
                   "Fri" -> fri
                   --"Sat" -> sat
    t = timeOfDayToTime (parseTimeOrError True defaultTimeLocale "%R" (Text.unpack $ Text.drop 4 time) :: TimeOfDay)
    s = UTCTime d t
    e = addUTCTime (secondsToNominalDiffTime (60*60)) s

-- getAvailability :: Text -> Spreadsheet -> [Person] -> [Person]
-- getAvailability tabName ss fac = fac'
--   where
--     (_:_:names):xs = map rowToStrings $ (head  $ ss ^. tab tabName . sData) ^. gdRowData
--     avails = concat $ map (\(s:e:xs') -> [(stringsToSlot s e, n, v) | (n, v) <- zip names xs', v /= ""]) (init xs)
--     fac' = map (\f -> f & availability ?~ [t | (t, n, v) <- avails, f ^. lastName == n]) fac


getAvailability :: Text -> Spreadsheet -> [Person] -> [Person]
getAvailability tabName ss fac = fac'
  where
    (_:_:firsts):(_:_:lasts):xs = map rowToStrings $ (head  $ ss ^. tab tabName . sData) ^. gdRowData
    names = zip firsts lasts
    
    avails = concat $ map (\(s:e:xs') -> [(stringsToSlot s e, n, v) | (n, v) <- zip names xs', v /= ""]) (init xs)
    fac' = map (\f -> f & availability ?~ [t | (t, (first, last), v) <- avails, first==f ^. firstName && last==f ^. lastName]) fac


defaultSlots gran = do
  let dates = ["2021-03-" ++ (show d) | d <- [8,9,10,11,12]]
  slots <- sequence $ [stringsToSlots d d "08:00" "19:00" gran | d <- dates]
  return $ concat slots



getPreferences :: Spreadsheet -> [Person] -> [Person] -> [Person]
getPreferences ss prosps fac = fac'
  where
    (_:_:_:names):xs = map rowToStrings $ (head  $ ss ^. tab "Individual Preferences" . sData) ^. gdRowData
    prefs = concat $ map (\(_:_:l:xs'') -> [(l, n, 1 :: Int) | (n, v) <- zip names xs'', v == "x"]) (init xs)
    plu = Map.fromList [(p ^. lastName, p) | p <- prosps]
    --fac' = map (\f -> f & preferences ?~ Map.fromList [((pf, pl), v) | (pf, pl, n, v) <- prefs, f ^. lastName == n]) fac
    fac' = map (\f -> f & preferences ?~ Map.fromList [(plu Map.! pl, v) | (pl, n, v) <- prefs, f ^. lastName == n]) fac
