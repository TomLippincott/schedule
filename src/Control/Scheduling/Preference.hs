{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Scheduling.Preference (Preference(..), interviewerParticipants, intervieweeParticipants, minutes, priority, simplePreference, required) where

import Text.Printf (printf, PrintfArg(..), fmtPrecision, fmtChar, errorBadFormat, formatString, vFmt, IsChar)
import Control.Lens ((^.), (.~), (<&>), set, view, makeLenses, makeFields)
--import Control.Scheduling.TimeSpan (TimeSpan(..))
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)
import Control.Lens

--data Gender = Male | Female | Other
--  deriving (Ord, Show, Eq)

data Preference = Preference { _interviewerParticipants :: Set (Text, Text)
                             , _intervieweeParticipants :: Set (Text, Text)
                             , _minutes :: Int
                             , _priority :: Int
                             , _required :: Bool
                             } deriving (Show, Eq, Ord)

simplePreference :: Preference
simplePreference = Preference Set.empty Set.empty 60 0 False
-- first last = Person first last Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 1 2 True

--instance PrintfArg Preference where
--  formatArg Preference{..} fmt = formatString (printf "%s %s" _firstName _lastName :: String) (fmt { fmtChar = 's', fmtPrecision = Nothing })
--  formatArg Person{..} fmt = formatString (printf "%s %s: Avail slots=%s, Req slots=%s" _firstName _lastName ((show . liftM length) _availability) ((show . liftM (sum . Map.elems)) _preferences) :: String) (fmt { fmtChar = 's', fmtPrecision = Nothing })  

makeLenses ''Preference

--fullName :: Lens' Person (Text, Text)
--fullName = lens (\p -> (p ^. firstName, p ^. lastName)) undefined
