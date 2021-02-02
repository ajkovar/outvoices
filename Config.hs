{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, OverloadedStrings, DeriveDataTypeable, CPP  #-}

module Config where

import System.Console.CmdArgs (cmdArgs)
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
import OutVoice (outvoice, OutVoice (timesheet_file, client_name))

data AppConfig = AppConfig {
  me :: Person,
  client :: Person,
  timesheets :: Vector Timesheet,
  font :: AnyFont,
  userArgs :: OutVoice
  }

loadConfig' :: ExceptT String IO AppConfig
loadConfig' = do
  userArgs <- ExceptT $ Right <$> cmdArgs outvoice
  let selfConfigFile = "data/me.json"
      clientConfigFile = "data/" ++ client_name userArgs ++ "/info.json"
  myConfig <- ExceptT $ Aeson.eitherDecode <$> readFile selfConfigFile
  clientConfig <- ExceptT $ Aeson.eitherDecode <$> readFile clientConfigFile
  csvData <- ExceptT $ decodeByName <$> readFile (timesheet_file userArgs)
  timesRoman <- ExceptT $ note "Error loading Times Roman font" <$> mkStdFont Times_Roman
  return AppConfig {
    me = myConfig,
    client = clientConfig,
    timesheets = snd csvData,
    font = timesRoman,
    userArgs = userArgs
    }

loadConfig :: IO (Either String AppConfig)
loadConfig = runExceptT loadConfig'

