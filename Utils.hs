
module Utils where

import Data.List.Split (chunksOf)
import Data.List (intercalate)
import Prelude hiding (drop)
import Data.Vector.Generic (Vector, drop, cons)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Split as VSplit

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
        rest = 20

  




