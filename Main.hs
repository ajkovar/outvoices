{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses,OverloadedStrings,DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Main where

import qualified Graphics.PDF as PDF
import Paths_HPDF
import qualified Data.Text as T
import Graphics.PDF.Typesetting
import Person
import System.IO 
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson
import qualified System.Console.CmdArgs as CArgs
import qualified Data.Csv as Csv
import qualified Data.Vector as V
import qualified Timesheet

import qualified Data.List.Split as Split
import qualified Data.List as List

format x = h++t
  where
    sp = break (== '.') $ show x
    h = reverse (List.intercalate "," $ Split.splitEvery 3 $ reverse $ fst sp) 
    t = case length $ snd sp of 3 -> (snd sp)
                                2 -> (snd sp) ++ "0"
                                _ -> (snd sp)

kingFisherDaisy :: PDF.Color
kingFisherDaisy = PDF.Rgb 0.32 0.11 0.52

data OutVoice = OutVoice {client_name :: String, 
                          issue_date :: String, 
                          due_date :: String,
                          invoice_number :: String,
                          timesheet_file :: String,
                          rate :: Double
                         } deriving (Show, CArgs.Data, CArgs.Typeable)

outvoice = OutVoice{
                      client_name = CArgs.def CArgs.&= CArgs.help "Client to generate for (should match their data directory name)",
                      issue_date = CArgs.def CArgs.&= CArgs.help "Issue date",
                      due_date = CArgs.def CArgs.&= CArgs.help "Due date",
                      invoice_number = CArgs.def CArgs.&= CArgs.help "Invoice Number",
                      timesheet_file = CArgs.def CArgs.&= CArgs.help "Location of CSV timesheet file",
                      rate = CArgs.def CArgs.&= CArgs.help "Hourly billing Rate"
                   } CArgs.&= CArgs.summary "Generate an invoice based on a csv input file"

setColor :: PDF.Color -> PDF.Draw ()
setColor color = do
  PDF.strokeColor color
  PDF.fillColor color

displayLine :: T.Text -> PDF.PDFText ()
displayLine t = do
  PDF.displayText t
  PDF.startNewLine

drawLine :: PDF.PDFFont -> T.Text -> Double -> Double -> PDF.Draw ()
drawLine theFont@(PDF.PDFFont f s) text x y = do
  PDF.drawText $ do
    PDF.setFont theFont
    PDF.textStart x y
    PDF.leading $ (PDF.getHeight f s) + 3
    PDF.renderMode PDF.FillText
    PDF.displayText text

drawLines :: PDF.PDFFont -> [T.Text] -> Double -> Double -> PDF.Draw [()]
drawLines theFont@(PDF.PDFFont f s) lines x y = do
  PDF.drawText $ do
    PDF.setFont theFont
    PDF.textStart x y
    PDF.leading $ (PDF.getHeight f s) + 3
    PDF.renderMode PDF.FillText
    mapM displayLine lines 
    --  stroke $ Rectangle (10 :+ (200.0 - (getDescent f s))) ((10.0 + textWidth theFont t) :+ (200.0 - getDescent f s + getHeight f s))

renderMyInfo :: Person -> PDF.AnyFont -> PDF.Draw [()]
renderMyInfo person timesRoman  = do
  setColor PDF.black
  let font = PDF.PDFFont timesRoman 10
  let nameAndNumber = (fmap (\f -> f person) [Person.name, Person.telephone])
  drawLines font nameAndNumber 400 730
  drawLines font (Person.addressFields person) 490 730

renderClientInfo :: Person -> PDF.AnyFont -> PDF.Draw [()]
renderClientInfo client timesRoman  = do
  let font = PDF.PDFFont timesRoman 10
  setColor kingFisherDaisy
  drawLine font "Billed To" 30 600
  setColor PDF.black
  let fields = [Person.name client] ++ (Person.addressFields client)
  drawLines font fields 30 587

renderTitledLine :: PDF.AnyFont -> T.Text -> T.Text -> Double -> Double -> PDF.Draw ()
renderTitledLine fontType title line x y = do
  let font = PDF.PDFFont fontType 10
  setColor kingFisherDaisy
  drawLine font title x y
  setColor PDF.black
  drawLine font line x (y - 13)

renderAmountDue :: PDF.AnyFont -> T.Text -> T.Text -> PDF.Draw ()
renderAmountDue fontType title line = do
  setColor kingFisherDaisy
  drawLine (PDF.PDFFont fontType 10) title 480 600
  setColor PDF.black
  drawLine (PDF.PDFFont fontType 19) line 480 (600 - 17)

renderRow ::PDF.AnyFont -> Double -> Int -> Timesheet.Timesheet -> PDF.Draw ()
renderRow fontType rate i timesheetItem = do
  let date = Timesheet.date timesheetItem
  let y = (480.00 - (fromIntegral i :: Double) * 60.00)
  let client = Timesheet.client timesheetItem
  let project = Timesheet.project timesheetItem
  let description = "(" ++ client ++ " - " ++ project ++ ") - " ++ date
  let hours = (Timesheet.hours timesheetItem)
  drawLine (PDF.PDFFont fontType 10) (T.pack "Time") 30 y
  drawLine (PDF.PDFFont fontType 9) (T.pack description) 30 (y - 12)
  drawLine (PDF.PDFFont fontType 9) (T.pack ((Timesheet.task timesheetItem) ++ " -")) 30 (y - 22)
  drawLine (PDF.PDFFont fontType 8) (T.pack (Timesheet.notes timesheetItem)) 30 (y - 34)
  drawLine (PDF.PDFFont fontType 8) (T.pack $ "$" ++ (format rate)) 400 y
  drawLine (PDF.PDFFont fontType 8) (T.pack (show hours)) 460 y
  drawLine (PDF.PDFFont fontType 8) (T.pack $ "$" ++ (format (rate * hours))) 520 y
  setColor $ PDF.Rgb 0.9 0.9 0.9
  PDF.stroke $ PDF.Line 30 (y - 50) 580 (y - 50)

main :: IO()
main = do
  userArgs <- CArgs.cmdArgs outvoice
  contents <- LBS.readFile "data/me.json"
  csvData <- LBS.readFile (timesheet_file userArgs)
  clientContents <- LBS.readFile $ "data/" ++ (client_name userArgs) ++ "/info.json"
  let (Just person) = Aeson.decode contents :: Maybe Person
  let (Just client) = Aeson.decode clientContents :: Maybe Person
  let rect = PDF.PDFRect 0 0 612 792
  -- runPdf "demo.pdf" (standardDocInfo { author=toPDFString "alpheccar", compressed = False}) rect $ do
  Just timesRoman <- PDF.mkStdFont PDF.Times_Roman 
  PDF.runPdf "demo.pdf" (PDF.standardDocInfo { PDF.author = "alex", PDF.compressed = False}) rect $ do
    page1 <- PDF.addPage Nothing
    PDF.newSection  "Text encoding" Nothing Nothing $ do
      PDF.drawWithPage page1 $ do
          renderMyInfo person timesRoman
          renderClientInfo client timesRoman
          renderTitledLine timesRoman "Date of Issue" (T.pack $ issue_date userArgs) 180 600
          renderTitledLine timesRoman "Due Date" (T.pack $ issue_date userArgs) 180 560
          renderTitledLine timesRoman "Invoice Number" (T.pack $ issue_date userArgs) 280 600
          setColor kingFisherDaisy
          PDF.stroke $ PDF.Line 30 520 580 520
          drawLine (PDF.PDFFont timesRoman 9) (T.pack "Description") 30 500
          drawLine (PDF.PDFFont timesRoman 9) (T.pack "Rate") 400 500
          drawLine (PDF.PDFFont timesRoman 9) (T.pack "Qty") 460 500
          drawLine (PDF.PDFFont timesRoman 9) (T.pack "Line Total") 520 500
          case Csv.decodeByName csvData of
            Left err -> return () -- System.IO.putStrLn err
            Right (_, v) -> do
              let total = V.foldr (\sheet s -> s + (Timesheet.hours sheet)) 0 v
              let amountDue = (T.pack $ "$" ++ format (total * (rate userArgs)))
              let zeroAmount = T.pack $ format 0.00
              renderAmountDue timesRoman "Amount Due" amountDue

              V.imapM (renderRow timesRoman (rate userArgs)) v

              let leftX = 420
              let y = fromIntegral (440 - (V.length v) * 50) :: Double
              setColor PDF.black
              drawLine (PDF.PDFFont timesRoman 10) (T.pack "Subtotal") leftX y
              drawLine (PDF.PDFFont timesRoman 10) amountDue 530 y
              drawLine (PDF.PDFFont timesRoman 10) (T.pack "Tax") leftX (y - 16)
              drawLine (PDF.PDFFont timesRoman 10) zeroAmount 530 (y - 16)

              setColor $ PDF.Rgb 0.9 0.9 0.9
              PDF.stroke $ PDF.Line 340 (y - 28) 580 (y - 28)

              setColor PDF.black
              drawLine (PDF.PDFFont timesRoman 10) (T.pack "Total") leftX (y - 44)
              drawLine (PDF.PDFFont timesRoman 10) amountDue 530 (y - 44)
              drawLine (PDF.PDFFont timesRoman 10) (T.pack "Amount Paid") leftX (y - 60)
              drawLine (PDF.PDFFont timesRoman 10) zeroAmount 530 (y - 60)

              setColor $ PDF.Rgb 0.9 0.9 0.9
              PDF.stroke $ PDF.Line 340 (y - 74) 580 (y - 74)
              PDF.stroke $ PDF.Line 340 (y - 76) 580 (y - 76)

              setColor kingFisherDaisy
              drawLine (PDF.PDFFont timesRoman 12) (T.pack "Amount Due") leftX (y - 94)
              setColor PDF.black
              drawLine (PDF.PDFFont timesRoman 10) amountDue 530 (y - 94)

              return ()
          return ()
        --   let style = Font (PDF.PDFFont timesRoman 8) PDF.red PDF.red
        --       rect = PDF.Rectangle (310 PDF.:+ 780) (510 PDF.:+ 790) 
        --       in displayFormattedText rect NormalParagraph style $ do 
        --         PDF.paragraph $ do
        --           txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor "
        --           txt $ "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud "
        --           txt $ "exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute "
        --           txt $ "irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
        --           txt $ "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia "
        --           txt $ "deserunt mollit anim id est laborum."