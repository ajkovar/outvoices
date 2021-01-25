{-# LANGUAGE OverloadedStrings #-}

module Timesheet where

import Data.Csv

data Timesheet = Timesheet
    { date   :: !String
    , client :: !String
    , project :: !String
    , task :: !String
    , notes :: !String
    , hours :: !Double
    } deriving (Show, Eq)

instance FromNamedRecord Timesheet where
    parseNamedRecord r = Timesheet <$> r .: "Date" <*> r .: "Client" <*> r .: "Project" <*> r .: "Task" <*> r .: "Notes" <*> r .: "Hours"