{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses, OverloadedStrings, CPP #-}

module Main where

import Graphics.PDF (
  Color, Draw, PDFText, AnyFont, black, strokeColor, fillColor, setFont, textStart, renderMode,
  getHeight, leading, Color(Rgb), black, displayText, startNewLine, author, PDFFont(PDFFont),
  compressed, stroke, Line(Line), runPdf, addPage, newSection, drawWithPage, TextMode(FillText),
  drawText, PDFRect(PDFRect), FontName(Times_Roman), standardDocInfo, PDF
  )
import Data.Text (Text, pack, unpack)
import qualified Person
import Person (Person(Person), name)
import System.Console.CmdArgs (cmdArgs)
import Prelude hiding (foldr, length)
import Data.Vector (Vector, foldr, imapM, length)
import qualified Timesheet
import Timesheet (Timesheet(Timesheet))
import OutVoice (OutVoice(OutVoice), outvoice, rate, invoice_number, due_date, client_name, issue_date, timesheet_file)
import Utils (formatMoney, paginate)
import Config (loadConfig, AppConfig(AppConfig), timesheets, me, client, font)
import Control.Monad (when)

kingFisherDaisy :: Color
kingFisherDaisy = Rgb 0.32 0.11 0.52

setColor :: Color -> Draw ()
setColor color = do
  strokeColor color
  fillColor color

displayLine :: Text -> PDFText ()
displayLine t = do
  displayText t
  startNewLine

drawLine :: PDFFont -> Text -> Double -> Double -> Draw ()
drawLine theFont@(PDFFont f s) text x y = do
  drawText $ do
    setFont theFont
    textStart x y
    leading $ getHeight f s + 3
    renderMode FillText
    displayText text

drawLines :: PDFFont -> [Text] -> Double -> Double -> Draw [()]
drawLines theFont@(PDFFont f s) lines x y = do
  drawText $ do
    setFont theFont
    textStart x y
    leading $ getHeight f s + 3
    renderMode FillText
    mapM displayLine lines

renderMyInfo :: Person -> AnyFont -> Double -> Double -> Draw [()]
renderMyInfo person timesRoman x y = do
  setColor black
  let font = PDFFont timesRoman 10
  let nameAndNumber = fmap (\f -> f person) [Person.name, Person.telephone]
  drawLines font nameAndNumber x y
  drawLines font (Person.addressFields person) (x+90) y

renderClientInfo :: Person -> AnyFont -> Double -> Double -> Draw [()]
renderClientInfo client timesRoman x y = do
  let font = PDFFont timesRoman 10
  setColor kingFisherDaisy
  drawLine font "Billed To" x y
  setColor black
  let fields = Person.name client : Person.addressFields client
  drawLines font fields x (y-13)

renderTitledLine :: AnyFont -> Text -> Text -> Double -> Double -> Draw ()
renderTitledLine fontType title line x y = do
  let font = PDFFont fontType 10
  setColor kingFisherDaisy
  drawLine font title x y
  setColor black
  drawLine font line x (y - 13)

renderAmountDue :: AnyFont -> Text -> Text -> Double -> Double -> Draw ()
renderAmountDue fontType title line x y = do
  setColor kingFisherDaisy
  drawLine (PDFFont fontType 10) title x y
  setColor black
  drawLine (PDFFont fontType 19) line x (y - 17)

renderRow :: AnyFont -> Double -> Double -> Int -> Timesheet -> Draw ()
renderRow fontType rate yInit i timesheetItem = do
  setColor black
  let date = Timesheet.date timesheetItem
      y = yInit - (fromIntegral i :: Double) * 65.00
      client = Timesheet.client timesheetItem
      project = Timesheet.project timesheetItem
      description = "(" ++ client ++ " - " ++ project ++ ") - " ++ date
      hours = Timesheet.hours timesheetItem
  drawLine (PDFFont fontType 10) (pack "Time") 30 y
  drawLine (PDFFont fontType 9) (pack description) 30 (y - 12)
  drawLine (PDFFont fontType 9) (pack (Timesheet.task timesheetItem ++ " -")) 30 (y - 22)
  drawLine (PDFFont fontType 8) (pack (Timesheet.notes timesheetItem)) 30 (y - 34)
  drawLine (PDFFont fontType 8) (pack $ "$" ++ formatMoney rate) 400 y
  drawLine (PDFFont fontType 8) (pack (show hours)) 460 y
  drawLine (PDFFont fontType 8) (pack $ "$" ++ formatMoney (rate * hours)) 520 y
  setColor $ Rgb 0.9 0.9 0.9
  stroke $ Line 30 (y - 45) 580 (y - 45)

renderFooter :: AnyFont -> Text -> Double -> Draw ()
renderFooter timesRoman amountDue y = do
  let leftX = 420
      zeroAmount = pack $ formatMoney 0.00
  setColor black
  drawLine (PDFFont timesRoman 10) (pack "Subtotal") leftX y
  drawLine (PDFFont timesRoman 10) amountDue 530 y
  drawLine (PDFFont timesRoman 10) (pack "Tax") leftX (y - 16)
  drawLine (PDFFont timesRoman 10) zeroAmount 530 (y - 16)

  setColor $ Rgb 0.9 0.9 0.9
  stroke $ Line 340 (y - 28) 580 (y - 28)

  setColor black
  drawLine (PDFFont timesRoman 10) (pack "Total") leftX (y - 44)
  drawLine (PDFFont timesRoman 10) amountDue 530 (y - 44)
  drawLine (PDFFont timesRoman 10) (pack "Amount Paid") leftX (y - 60)
  drawLine (PDFFont timesRoman 10) zeroAmount 530 (y - 60)

  setColor $ Rgb 0.9 0.9 0.9
  stroke $ Line 340 (y - 74) 580 (y - 74)
  stroke $ Line 340 (y - 76) 580 (y - 76)

  setColor kingFisherDaisy
  drawLine (PDFFont timesRoman 12) (pack "Amount Due") leftX (y - 94)
  setColor black
  drawLine (PDFFont timesRoman 10) amountDue 530 (y - 94)

renderHeader :: AnyFont -> Person -> Person -> Text -> Double -> OutVoice -> Draw ()
renderHeader timesRoman person client amountDue height userArgs = do
  renderMyInfo person timesRoman 400 (height-60)
  renderClientInfo client timesRoman 30 (height-200)
  renderTitledLine timesRoman "Date of Issue" (pack $ issue_date userArgs) 180 (height-200)
  renderTitledLine timesRoman "Due Date" (pack $ due_date userArgs) 180 (height-240)
  renderTitledLine timesRoman "Invoice Number" (pack $ invoice_number userArgs) 280 (height-200)
  setColor kingFisherDaisy
  stroke $ Line 30 (height-280) 580 (height-280)
  drawLine (PDFFont timesRoman 9) (pack "Description") 30 (height-300)
  drawLine (PDFFont timesRoman 9) (pack "Rate") 400 (height-300)
  drawLine (PDFFont timesRoman 9) (pack "Qty") 460 (height-300)
  drawLine (PDFFont timesRoman 9) (pack "Line Total") 520 (height-300)
  renderAmountDue timesRoman "Amount Due" amountDue 480 (height-200)

renderPage :: AppConfig -> [Vector Timesheet] -> Text -> OutVoice -> Double -> Vector Timesheet -> PDF ()
renderPage config allEntries amountDue userArgs height pageEntries = do
  let isFirstPage = pageEntries == head allEntries
      isLastPage = pageEntries == last allEntries
      rowYInit = if isFirstPage then height - 320 else height - 60
      y = rowYInit - (fromIntegral (length pageEntries + 1) :: Double) * 65.00
      maxRows = if isFirstPage then 6 else 11
      isEnoughSpaceForFooter =  length pageEntries < maxRows
      fontType = font config
  page <- addPage Nothing
  newSection  "Text encoding" Nothing Nothing $ do
    drawWithPage page $ do
      when isFirstPage $ renderHeader fontType (me config) (client config) amountDue height userArgs
      imapM (renderRow fontType (rate userArgs) rowYInit) pageEntries
      when (isEnoughSpaceForFooter && isLastPage) $ renderFooter fontType amountDue y
  when (not isEnoughSpaceForFooter && isLastPage) $ do
    page <- addPage Nothing
    newSection  "Text encoding" Nothing Nothing $ do
      drawWithPage page $ do
        renderFooter fontType amountDue (height - 60)

generatePdf :: AppConfig -> OutVoice -> IO ()
generatePdf config userArgs = do
  let totalHeight = 892
      paginatedEntries = paginate (timesheets config)
      total = foldr (\sheet s -> s + Timesheet.hours sheet) 0 (timesheets config)
      amountDue = pack $ "$" ++ formatMoney (total * rate userArgs)
      myName = (Person.name . me) config
      rect = PDFRect 0 0 612 totalHeight
      outFile = "data/" ++ client_name userArgs ++ "/invoices/" ++ unpack myName ++ " Invoice " ++ invoice_number userArgs ++ ".pdf"
  putStrLn "Generating output file:"
  putStrLn outFile
  runPdf outFile (standardDocInfo { author = myName, compressed = False}) rect $
    mapM (renderPage config paginatedEntries amountDue userArgs totalHeight) paginatedEntries

main :: IO ()
main = do
  userArgs <- cmdArgs outvoice
  loadedData <- loadConfig (client_name userArgs) (timesheet_file userArgs)
  case loadedData of
    Left error -> putStrLn error
    Right config -> generatePdf config userArgs
















