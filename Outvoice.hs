{-# LANGUAGE DeriveDataTypeable #-}

module OutVoice where

import System.Console.CmdArgs (def, help, (&=), Data, Typeable, summary, name, Default)

data BillingCycle = Monthly | Semimonthly deriving (Read, Show, Data)

instance Default BillingCycle where def = Monthly

data OutVoice = OutVoice {
  client_name :: String, 
  billingCycle :: BillingCycle, 
  -- issue_date :: String, 
  invoice_number :: String,
  timesheet_file :: String,
  rate :: Double
  } deriving (Show, Data, Typeable)

outvoice = OutVoice {
   client_name = def &= help "Client to generate for (should match their data directory name)",
   billingCycle = def &= name "billing-cycle" &= help "Type of billing cycle (Monthly or SemiMonthly)",
  --  issue_date = def &= help "Issue date",
   invoice_number = def &= help "Invoice Number",
   timesheet_file = def &= help "Location of CSV timesheet file",
   rate = def &= help "Hourly billing Rate"
} &= summary "Generate an invoice based on a csv input file"
