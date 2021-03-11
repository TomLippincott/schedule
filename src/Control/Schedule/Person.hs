{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Schedule.Person ( Person(..)
                               , Gender(..)
                               , simplePerson
                               , firstName
                               , lastName
                               , email
                               --, school
                               --, office
                               , availability
                               , preferences
                               --, gender
                               --, urm
                               --, application
                               --, local
                               --, contact
                               --, ref
                               --, biography
                               , fullName
                               , zoom
                               , maxMeetings
                               , minMeetings
                               , maxMeetingSize
                               , requestedOnly
                               , application
                               , group
                               ) where

import Text.Printf (printf, PrintfArg(..), fmtPrecision, fmtChar, errorBadFormat, formatString, vFmt, IsChar)
import Control.Lens ((^.), (.~), (<&>), set, view, makeLenses, makeFields)
import Control.Schedule.TimeSpan (TimeSpan(..))
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad (liftM)
import Control.Lens hiding (zoom)
import Data.Text (pack, unpack, intercalate)

data Gender = Male | Female | Other
  deriving (Ord, Show, Eq)

data Person = Person { _firstName :: Text
                     , _lastName :: Text
                     , _email :: Maybe Text
                     --, _school :: Maybe Text
                     --, _office :: Maybe Text
                     , _availability :: Maybe [TimeSpan]
                     , _preferences :: Maybe (Map Person Int)
                     --, _gender :: Maybe Gender
                     --, _urm :: Maybe Bool
                     , _application :: Maybe Text
                     --, _local :: Maybe Bool
                     --, _contact :: Maybe Person
                     --, _ref :: Maybe Text
                     --, _biography :: Maybe Text
                     , _zoom :: Maybe Text
                     , _maxMeetings :: Int
                     , _minMeetings :: Int
                     , _maxMeetingSize :: Int
                     , _requestedOnly :: Bool
                     , _group :: Maybe Text
                     } deriving (Eq, Ord)

simplePerson :: Text -> Text -> Person
simplePerson first last = Person first last Nothing Nothing Nothing Nothing Nothing 3 1 2 True Nothing
--simplePerson first last = Person first last Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 1 2 True

instance PrintfArg Person where
  formatArg Person{..} fmt = formatString (printf "%s %s" _firstName _lastName :: String) (fmt { fmtChar = 's', fmtPrecision = Nothing })
--  formatArg Person{..} fmt = formatString (printf "%s %s: Avail slots=%s, Req slots=%s" _firstName _lastName ((show . liftM length) _availability) ((show . liftM (sum . Map.elems)) _preferences) :: String) (fmt { fmtChar = 's', fmtPrecision = Nothing })  


makeLenses ''Person

instance Show Person where
  show p = (unpack . intercalate " ") [p^.firstName, p^.lastName, (pack . show) (p^.minMeetings), (pack . show) (p^.maxMeetings)]

fullName :: Lens' Person (Text, Text)
fullName = lens (\p -> (p ^. firstName, p ^. lastName)) undefined
