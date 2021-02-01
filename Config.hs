{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, OverloadedStrings, DeriveDataTypeable, CPP  #-}

module Config where

import Graphics.PDF (AnyFont, FontName(Times_Roman), mkStdFont)
import Person (Person(Person))
import Prelude hiding (readFile)
import Data.ByteString.Lazy (readFile)
import qualified Data.Aeson as Aeson
import Data.Csv (decodeByName)
import Data.Vector (Vector)
import qualified Timesheet
import Timesheet (Timesheet(Timesheet))
import Control.Monad.Trans.Except (runExceptT, ExceptT(ExceptT))
import Control.Error.Util (note)

data AppConfig = AppConfig {
  me :: Person, 
  client :: Person, 
  timesheets :: Vector Timesheet, 
  font :: AnyFont
  }

loadConfig' :: String -> String -> ExceptT String IO AppConfig
loadConfig' clientName timesheetFile = do
  let selfConfigFile = "data/me.json"
      clientConfigFile = "data/" ++ clientName ++ "/info.json"
  myConfig <- ExceptT $ Aeson.eitherDecode <$> readFile selfConfigFile
  clientConfig <- ExceptT $ Aeson.eitherDecode <$> readFile clientConfigFile
  csvData <- ExceptT $ decodeByName <$> readFile timesheetFile
  timesRoman <- ExceptT $ note "Error loading Times Roman font" <$> mkStdFont Times_Roman 
  return AppConfig {
    me = myConfig, 
    client = clientConfig, 
    timesheets = snd csvData, 
    font = timesRoman
    }

loadConfig :: String -> String -> IO (Either String AppConfig)
loadConfig client timesheetFile = runExceptT $ loadConfig' client timesheetFile
