{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, OverloadedStrings, CPP #-}

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
import OutVoice (outvoice, OutVoice (timesheet_file, client_name, billingCycle), BillingCycle (Monthly, Semimonthly))
import Data.Time ( Day, UTCTime(utctDay), fromGregorian )
import Data.Time.Calendar ( toGregorian, addGregorianMonthsClip )
import Data.Time.Clock (getCurrentTime)
import Utils (curentInvoiceNumber, getLatestTimesheet)
import Data.Maybe (fromMaybe)
import Control.Monad (when)
import Control.Error (isNothing)

data AppConfig = AppConfig {
  me :: Person,
  client :: Person,
  timesheets :: Vector Timesheet,
  font :: AnyFont,
  userArgs :: OutVoice,
  issueDate :: Day,
  dueDate :: Day,
  invoiceNumber :: Int
  }

loadConfig' :: ExceptT String IO AppConfig
loadConfig' = do
  userArgs <- ExceptT $ Right <$> cmdArgs outvoice
  let selfConfigFile = "data/me.json"
      clientConfigFile = "data/" ++ client_name userArgs ++ "/info.json"
  myConfig <- ExceptT $ Aeson.eitherDecode <$> readFile selfConfigFile
  clientConfig <- ExceptT $ Aeson.eitherDecode <$> readFile clientConfigFile
  latestTimesheet <- ExceptT $ Right <$> getLatestTimesheet (client_name userArgs)
  csvData <- ExceptT $ decodeByName <$> readFile (fromMaybe latestTimesheet (timesheet_file userArgs))
  timesRoman <- ExceptT $ note "Error loading Times Roman font" <$> mkStdFont Times_Roman
  ExceptT $ Right <$> when (isNothing (timesheet_file userArgs)) 
    (putStrLn ("No timesheet option specified.  Using last updated timesheet:\n" ++ latestTimesheet))
  (year, month, day) <- ExceptT $ Right . toGregorian . utctDay <$> getCurrentTime
  let issueDate = case billingCycle userArgs of
                     Monthly -> fromGregorian year month 1
                     Semimonthly -> fromGregorian year month (if day > 15 then 16 else 1)
      dueDate = addGregorianMonthsClip 1 issueDate
  number <- ExceptT $ Right <$> curentInvoiceNumber (client_name userArgs)

  return AppConfig {
    me = myConfig,
    client = clientConfig,
    timesheets = snd csvData,
    font = timesRoman,
    userArgs = userArgs,
    issueDate = issueDate,
    dueDate = dueDate,
    invoiceNumber = number
    }

loadConfig :: IO (Either String AppConfig)
loadConfig = runExceptT loadConfig'