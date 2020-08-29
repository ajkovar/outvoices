{-# LANGUAGE ScopedTypeVariables, MultiParamTypeClasses,OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Main where

import Graphics.PDF
import Paths_HPDF
import qualified Data.Text as T
-- import Graphics.PDF.LowLevel.Types

-- | Create a PDF string from an Haskell one
-- toPDFString :: T.Text -> PDFString
-- toPDFString = PDFString . encodeUtf16BE

fontDebug :: PDFFont -> T.Text -> Draw ()
fontDebug theFont@(PDFFont f s) t = do
     drawText $ do
         setFont theFont
         textStart 10 10.0
         leading $ getHeight f s
         renderMode FillText
         displayText t
        --  startNewLine
        --  displayText "Another little test"
    --  strokeColor $ Rgb 1 0 0
    --  stroke $ Line 10 200 612 200
    --  fill $ Circle 10 200 10
    --  stroke $ Rectangle (10 :+ (200.0 - (getDescent f s))) ((10.0 + textWidth theFont t) :+ (200.0 - getDescent f s + getHeight f s))

textTest :: AnyFont -> Draw ()
textTest timesRoman  = do
    strokeColor red
    fillColor blue
    fontDebug (PDFFont timesRoman 48) ("This is a \\test (éèçàù)!")

main :: IO()
main = do
    let rect = PDFRect 0 0 600 400
    -- runPdf "demo.pdf" (standardDocInfo { author=toPDFString "alpheccar", compressed = False}) rect $ do
    Just timesRoman <- mkStdFont Times_Roman 
    runPdf "demo.pdf" (standardDocInfo { author = "alex", compressed = False}) rect $ do
        page1 <- addPage Nothing
        newSection  "Text encoding" Nothing Nothing $ do
            drawWithPage page1 $ do
               textTest timesRoman