{-# LANGUAGE DeriveDataTypeable #-}
module OutVoice where

import System.Console.CmdArgs (def, help, (&=), Data, Typeable, summary)

data OutVoice = OutVoice {
  client_name :: String, 
  issue_date :: String, 
  due_date :: String,
  invoice_number :: String,
  timesheet_file :: String,
  rate :: Double
  } deriving (Show, Data, Typeable)

outvoice = OutVoice {
   client_name = def &= help "Client to generate for (should match their data directory name)",
   issue_date = def &= help "Issue date",
   due_date = def &= help "Due date",
   invoice_number = def &= help "Invoice Number",
   timesheet_file = def &= help "Location of CSV timesheet file",
   rate = def &= help "Hourly billing Rate"
} &= summary "Generate an invoice based on a csv input file"
