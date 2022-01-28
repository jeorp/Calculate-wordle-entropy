module Wordle where


import Data.Char
import Data.List
import qualified Data.List.Key as LTH
import Data.Maybe 
import Control.Arrow 

data Color = Gray | Yellow | Green deriving(Show, Eq)

judgeWordle :: String -> String -> [Color]
judgeWordle a b = if length a == length b then cal a b else []
  where
    cal' xs (a:as) (b:bs) cols
     | a == b = cal' xs as bs (Green:cols)
     | b `elem` xs = cal' xs as bs (Yellow:cols)
     | otherwise = cal' xs as bs (Gray:cols)
    cal' _ _ _ cols = cols

    cal a b = reverse $ cal' a a b []

wordleEntropyAverage :: String -> [String] -> Double
wordleEntropyAverage s dict = undefined