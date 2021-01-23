
module Utils where

import Data.List.Split (splitEvery)
import Data.List (intercalate)

formatMoney :: Double -> String
formatMoney x = h++t
  where
    sp = break (== '.') $ show x
    h = reverse (intercalate "," $ splitEvery 3 $ reverse $ fst sp) 
    decimal = snd sp
    t = case length decimal of 
      3 -> decimal
      2 -> decimal ++ "0"
      _ -> take 3 decimal