
module Utils where

import Data.List.Split (chunksOf)
import Data.List (intercalate)
import Prelude hiding (drop)
import Data.Vector.Generic (Vector, drop, cons)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Split as VSplit
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import System.Directory (listDirectory, getModificationTime)
import Control.Error ( mapMaybe )
import System.FilePath ((</>))

escapeSpaces :: String -> String
escapeSpaces = foldr (\ x -> (++) (if x == ' ' then "\\ " else [x])) ""

extractNumbers :: String -> Maybe Int
extractNumbers xs = readMaybe $ foldr (\ x -> (++) ([x | isDigit x])) "" xs

curentInvoiceNumber :: String -> IO Int
curentInvoiceNumber client = do
  files <- listDirectory $ "." </> "data" </> client </> "invoices"
  let versions = mapMaybe extractNumbers files
  case versions of
    [] -> return 1
    xs -> return $ maximum xs + 1

getLatestTimesheet :: String -> IO FilePath
getLatestTimesheet client = do
  let directory = "." </> "data" </> client </> "timesheets"
  files <- fmap (directory </>) . filter (\x -> head x /= '.') <$> listDirectory directory
  modificationTimes <- mapM getModificationTime files
  return $ snd $ maximum $ zip modificationTimes files

formatMoney :: Double -> String
formatMoney x = h++t
  where
    sp = break (== '.') $ show x
    h = reverse (intercalate "," $ chunksOf 3 $ reverse $ fst sp)
    decimal = snd sp
    t = case length decimal of
      3 -> decimal
      2 -> decimal ++ "0"
      _ -> take 3 decimal

paginate :: Vector v a => v a -> [v a]
paginate v
  | V.length v > firstPage = V.take firstPage v : VSplit.chunksOf rest (drop rest v)
  | otherwise = [v]
  where firstPage = 8
        rest = 12






