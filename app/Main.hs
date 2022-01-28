module Main where

import Wordle
import Data.List (intercalate)

main = do
  putStrLn "Please wait ..."
  wordleEntropyRanking "wordle.txt" 
  >>= writeFile "wordleEntropyRanking.txt" . intercalate "\n" . fmap show
  >> putStrLn "Finish!"