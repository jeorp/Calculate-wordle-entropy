module Main where

import Wordle
import Data.List (intercalate)

main = do
  putStrLn "Please wait ..."
  wordleEntropyRanking "wordle.txt" 
  >>= writeFile "wordleEntropyRanking.txt" . intercalate "\n" . fmap fst . take 100
  >> putStrLn "Finish!"