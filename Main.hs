{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses,OverloadedStrings,DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

module Main where

import Graphics.PDF (
  Color, Draw, PDFText, AnyFont, black, strokeColor, fillColor, setFont, textStart, renderMode, 
  getHeight, leading, Color(Rgb), black, displayText, startNewLine, author, PDFFont(PDFFont), 
  compressed, stroke, Line(Line), runPdf, addPage, newSection, drawWithPage, TextMode(FillText), 
  drawText, mkStdFont, PDFRect(PDFRect), FontName(Times_Roman), standardDocInfo
  )
import Paths_HPDF
import Data.Text (Text, pack)
import Graphics.PDF.Typesetting
import Person
import Prelude hiding (readFile)
import Data.ByteString.Lazy (readFile)
import qualified Data.Aeson as Aeson
import System.Console.CmdArgs (def, help, (&=), Data, Typeable, summary, cmdArgs)
import Data.Csv (decodeByName)
import qualified Data.Vector as V
import qualified Timesheet
import qualified Data.List.Split as Split
import qualified Data.List as List
import Control.Monad.Trans.Except (runExceptT, ExceptT(ExceptT))
import Control.Error.Util (note)

format x = h++t
  where
    sp = break (== '.') $ show x
    h = reverse (List.intercalate "," $ Split.splitEvery 3 $ reverse $ fst sp) 
    decimal = snd sp
    t = case length decimal of 
      3 -> decimal
      2 -> decimal ++ "0"
      _ -> take 3 decimal

kingFisherDaisy :: Color
kingFisherDaisy = Rgb 0.32 0.11 0.52

data OutVoice = OutVoice {
  client_name :: String, 
  issue_date :: String, 
  due_date :: String,
  invoice_number :: String,
  timesheet_file :: String,
  rate :: Double
  } deriving (Show, Data, Typeable)

outvoice = OutVoice{
   client_name = def &= help "Client to generate for (should match their data directory name)",
   issue_date = def &= help "Issue date",
   due_date = def &= help "Due date",
   invoice_number = def &= help "Invoice Number",
   timesheet_file = def &= help "Location of CSV timesheet file",
   rate = def &= help "Hourly billing Rate"
} &= summary "Generate an invoice based on a csv input file"

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
    leading $ (getHeight f s) + 3
    renderMode FillText
    displayText text

drawLines :: PDFFont -> [Text] -> Double -> Double -> Draw [()]
drawLines theFont@(PDFFont f s) lines x y = do
  drawText $ do
    setFont theFont
    textStart x y
    leading $ (getHeight f s) + 3
    renderMode FillText
    mapM displayLine lines 
    --  stroke $ Rectangle (10 :+ (200.0 - (getDescent f s))) ((10.0 + textWidth theFont t) :+ (200.0 - getDescent f s + getHeight f s))

renderMyInfo :: Person -> AnyFont -> Double -> Double -> Draw [()]
renderMyInfo person timesRoman x y = do
  setColor black
  let font = PDFFont timesRoman 10
  let nameAndNumber = (fmap (\f -> f person) [Person.name, Person.telephone])
  drawLines font nameAndNumber x y
  drawLines font (Person.addressFields person) (x+90) y

renderClientInfo :: Person -> AnyFont -> Double -> Double -> Draw [()]
renderClientInfo client timesRoman x y = do
  let font = PDFFont timesRoman 10
  setColor kingFisherDaisy
  drawLine font "Billed To" x y
  setColor black
  let fields = [Person.name client] ++ (Person.addressFields client)
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

renderRow :: AnyFont -> Double -> Double -> Int -> Timesheet.Timesheet -> Draw ()
renderRow fontType rate yInit i timesheetItem = do
  setColor black
  let date = Timesheet.date timesheetItem
  let y = (yInit - (fromIntegral i :: Double) * 65.00)
  let client = Timesheet.client timesheetItem
  let project = Timesheet.project timesheetItem
  let description = "(" ++ client ++ " - " ++ project ++ ") - " ++ date
  let hours = (Timesheet.hours timesheetItem)
  drawLine (PDFFont fontType 10) (pack "Time") 30 y
  drawLine (PDFFont fontType 9) (pack description) 30 (y - 12)
  drawLine (PDFFont fontType 9) (pack ((Timesheet.task timesheetItem) ++ " -")) 30 (y - 22)
  drawLine (PDFFont fontType 8) (pack (Timesheet.notes timesheetItem)) 30 (y - 34)
  drawLine (PDFFont fontType 8) (pack $ "$" ++ (format rate)) 400 y
  drawLine (PDFFont fontType 8) (pack (show hours)) 460 y
  drawLine (PDFFont fontType 8) (pack $ "$" ++ (format (rate * hours))) 520 y
  setColor $ Rgb 0.9 0.9 0.9
  stroke $ Line 30 (y - 45) 580 (y - 45)

type AppConfig = (Person, Person, V.Vector Timesheet.Timesheet, AnyFont)

loadConfig' :: String -> ExceptT String IO AppConfig
loadConfig' client = do
  let selfConfigFile = "data/me.json"
      clientConfigFile = "data/" ++ client ++ "/info.json"
  myConfig <- ExceptT $ Aeson.eitherDecode <$> readFile selfConfigFile
  clientConfig <- ExceptT $ Aeson.eitherDecode <$> readFile clientConfigFile
  csvData <- ExceptT $ decodeByName <$> readFile ""
  timesRoman <- ExceptT $ note "Error loading font" <$> mkStdFont Times_Roman 
  return (myConfig, clientConfig, snd csvData, timesRoman)

loadConfig :: String -> IO (Either String AppConfig)
loadConfig client = runExceptT $ loadConfig' client

main :: IO()
main = do
  let height = 892
      rect = PDFRect 0 0 612 height
  userArgs <- cmdArgs outvoice
  loadedData <- loadConfig (client_name userArgs)
  case loadedData of 
    Left error -> putStrLn error
    Right (person, client, v, timesRoman) -> do
      -- runPdf "demo.pdf" (standardDocInfo { author=toPDFString "alpheccar", compressed = False}) rect $ do
      runPdf "demo.pdf" (standardDocInfo { author = "alex", compressed = False}) rect $ do
        page1 <- addPage Nothing
        newSection  "Text encoding" Nothing Nothing $ do
          drawWithPage page1 $ do
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
            let total = V.foldr (\sheet s -> s + (Timesheet.hours sheet)) 0 v
            let amountDue = (pack $ "$" ++ format (total * (rate userArgs)))
            let zeroAmount = pack $ format 0.00
            renderAmountDue timesRoman "Amount Due" amountDue 480 (height-200)
            let rowYInit = height - 320
            V.imapM (renderRow timesRoman (rate userArgs) rowYInit) v
    
            let leftX = 420
            let y = rowYInit - ((fromIntegral (V.length v + 1)) :: Double) * 65.00
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
    
            return ()
            --   let style = Font (PDFFont timesRoman 8) red red
            --       rect = Rectangle (310 :+ 780) (510 :+ 790) 
            --       in displayFormattedText rect NormalParagraph style $ do 
            --         paragraph $ do
            --           txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor "
            --           txt $ "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud "
            --           txt $ "exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute "
            --           txt $ "irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
            --           txt $ "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia "
            --           txt $ "deserunt mollit anim id est laborum."