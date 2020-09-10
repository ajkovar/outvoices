{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses,OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main where

import qualified Graphics.PDF as PDF
import Paths_HPDF
import qualified Data.Text as T
import Graphics.PDF.Typesetting
import Person
import System.IO 
import Data.ByteString.Lazy as LBS
import Data.Aeson as Aeson
-- import Graphics.PDF.LowLevel.Types

-- | Create a PDF string from an Haskell one
-- toPDFString :: T.Text -> PDFString
-- toPDFString = PDFString . encodeUtf16BE

renderLine :: T.Text -> PDF.PDFText ()
renderLine t = do
  PDF.displayText t
  PDF.startNewLine

renderLines :: PDF.PDFFont -> [T.Text] -> Double -> Double -> PDF.Draw [()]
renderLines theFont@(PDF.PDFFont f s) lines x y = do
  PDF.drawText $ do
    PDF.setFont theFont
    PDF.textStart x y
    PDF.leading $ (PDF.getHeight f s) + 3
    PDF.renderMode PDF.FillText
    mapM renderLine lines 
    --  strokeColor $ Rgb 1 0 0
    --  stroke $ Line 10 200 612 200
    --  stroke $ Rectangle (10 :+ (200.0 - (getDescent f s))) ((10.0 + textWidth theFont t) :+ (200.0 - getDescent f s + getHeight f s))

renderMyInfo :: Person -> PDF.AnyFont -> PDF.Draw [()]
renderMyInfo person timesRoman  = do
  PDF.strokeColor PDF.black
  PDF.fillColor PDF.black
  let font = PDF.PDFFont timesRoman 10
  let nameAndNumber = (fmap (\f -> f person) [Person.name, Person.telephone])
  renderLines font nameAndNumber 400 750
  renderLines font (Person.addressFields person) 500 750

main :: IO()
main = do
  contents <- LBS.readFile "data/me.json"
  let (Just person) = Aeson.decode contents :: Maybe Person
  let rect = PDF.PDFRect 0 0 612 792
  -- runPdf "demo.pdf" (standardDocInfo { author=toPDFString "alpheccar", compressed = False}) rect $ do
  Just timesRoman <- PDF.mkStdFont PDF.Times_Roman 
  PDF.runPdf "demo.pdf" (PDF.standardDocInfo { PDF.author = "alex", PDF.compressed = False}) rect $ do
    page1 <- PDF.addPage Nothing
    PDF.newSection  "Text encoding" Nothing Nothing $ do
      PDF.drawWithPage page1 $ do
          renderMyInfo person timesRoman
          PDF.drawText $ do PDF.startNewLine
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