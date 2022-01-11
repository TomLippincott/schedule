{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Scheduling.Location (Location(..)) where

import Text.Printf (printf, PrintfArg(..), fmtPrecision, fmtChar, errorBadFormat, formatString, vFmt, IsChar)
import Control.Lens ((^.), (.~), (<&>), set, view, makeLenses, makeFields)
import Data.Text (Text)
import Data.Map (Map)

data Location = Location { _building :: String
                         , _room :: String
                         , _availability :: Maybe [Int] --[TimeSpan]
                         , _capacity :: Maybe Int
                         }

makeFields ''Location
