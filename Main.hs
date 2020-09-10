{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses,OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main where

import Graphics.PDF
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

renderLine :: T.Text -> PDFText ()
renderLine t = do
  displayText t
  startNewLine

renderLines :: PDFFont -> [T.Text] -> Draw [()]
renderLines theFont@(PDFFont f s) lines = do
  drawText $ do
    setFont theFont
    textStart 10 702.0
    leading $ getHeight f s
    renderMode FillText
    mapM renderLine lines 
    -- let renderLine t = do
    --     displayText t
    --     startNewLine
    -- in mapM renderLine lines
    --  strokeColor $ Rgb 1 0 0
    --  stroke $ Line 10 200 612 200
    --  fill $ Circle 10 200 10
    --  stroke $ Rectangle (10 :+ (200.0 - (getDescent f s))) ((10.0 + textWidth theFont t) :+ (200.0 - getDescent f s + getHeight f s))

renderMyInfo :: Person -> AnyFont -> Draw [()]
renderMyInfo person timesRoman  = do
  -- (Person.name $ person)
  strokeColor red
  fillColor blue
  renderLines (PDFFont timesRoman 12) (fmap (\f -> f person) [Person.name, Person.address])

main :: IO()
main = do
  contents <- LBS.readFile "data/me.json"
  let (Just person) = Aeson.decode contents :: Maybe Person
  let rect = PDFRect 0 0 612 792
  -- runPdf "demo.pdf" (standardDocInfo { author=toPDFString "alpheccar", compressed = False}) rect $ do
  Just timesRoman <- mkStdFont Times_Roman 
  runPdf "demo.pdf" (standardDocInfo { author = "alex", compressed = False}) rect $ do
    page1 <- addPage Nothing
    newSection  "Text encoding" Nothing Nothing $ do
      drawWithPage page1 $ do
          renderMyInfo person timesRoman
          drawText $ do startNewLine
          let style = Font (PDFFont timesRoman 8) red red
              rect = Rectangle (310 :+ 780) (510 :+ 790) 
              in displayFormattedText rect NormalParagraph style $ do 
                paragraph $ do
                  txt $ "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor "
                  txt $ "incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud "
                  txt $ "exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute "
                  txt $ "irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
                  txt $ "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia "
                  txt $ "deserunt mollit anim id est laborum."