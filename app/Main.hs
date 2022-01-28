module Main where

--import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char
import Data.List
import qualified Data.List.Key as LTH
import Data.Maybe 
import Control.Arrow 

main :: IO ()
main = do
  pure ()

validate :: FilePath -> FilePath -> IO ()
validate input output = do
  file <- readFile input
  let ws = words file
      five_words = filter (all isAlpha) ws
  print $ length ws
  writeFile output $ intercalate "\n" five_words

entropyAverage :: String -> Double
entropyAverage s = sum $ uncurry (*) . (charPossibility &&& charEntropy) <$> s
 
possibilityMap :: [(Char, Double)]
possibilityMap = 
 [
  ('a', 0.0575),
  ('b', 0.0128),
  ('c', 0.0263),
  ('d', 0.0285),
  ('e', 0.0913),
  ('f', 0.0173),
  ('g', 0.0133),
  ('h', 0.0313),
  ('i', 0.0599),
  ('j', 0.0006),
  ('k', 0.0084),
  ('l', 0.0335),
  ('m', 0.0235),
  ('n', 0.0596),
  ('o', 0.0689),
  ('p', 0.0192),
  ('q', 0.0008),
  ('r', 0.0508),
  ('s', 0.0567),
  ('t', 0.0706),
  ('u', 0.0334),
  ('v', 0.0069),
  ('w', 0.0119),
  ('x', 0.0073),
  ('y', 0.0164),
  ('z', 0.0007) 
 ]

charPossibility :: Char -> Double
charPossibility c = if isAlpha c
  then fromMaybe 1  $ lookup (toLower c) possibilityMap
  else 1

charEntropy :: Char -> Double
charEntropy = negate . logBase 2 . charPossibility

ranking :: FilePath -> IO [(String, Double)]
ranking path = do
  file <-readFile path
  let ws = words file
      res = (id &&& entropyAverage) <$> ws
  return $ LTH.sort (negate . snd) res