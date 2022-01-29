module Wordle where

import Control.Concurrent.Async
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import qualified Data.List.Key as LTH
import qualified Data.Map.Strict as M
 
import Data.Monoid (Endo(Endo, appEndo))
import Control.Applicative
import Control.Arrow (Arrow((&&&))) 

data Color = Gray | Yellow | Green deriving(Show, Eq)

instance Ord Color where
  compare Green Green = EQ
  compare Green _ = GT
  compare Yellow Yellow = EQ
  compare Yellow _ = GT
  compare Gray Gray = EQ
  compare _ _ = LT

data StringPair = StringPair !String !String deriving(Show)

instance Eq StringPair where
  (StringPair a b) == (StringPair c d) 
   | (a == c && b == d) || (a == d && b == c) = True
   | otherwise = False

instance Ord StringPair where
  compare s1@(StringPair a b) s2@(StringPair c d)
   | s1 == s2 = EQ
   | a == c = compare b d
   | otherwise = compare a c  


judgeWordle :: String -> String -> [Color]
judgeWordle a b = if length a == length b then cal a b else []
  where
    cal' xs (a:as) (b:bs) cols
     | a == b = cal' xs as bs (Green:cols)
     | b `elem` xs = cal' xs as bs (Yellow:cols)
     | otherwise = cal' xs as bs (Gray:cols)
    cal' _ _ _ cols = cols

    cal a b = reverse $ cal' a a b []

judgeWordleDynamic :: String -> TVar (M.Map StringPair [Color]) -> String -> IO [Color]
judgeWordleDynamic a var b = do
  let pair = StringPair a b
  m <- readTVarIO var 
  case M.lookup pair m of
    Just colors -> do
      atomically $ modifyTVar' var (M.delete pair)
      pure colors
    Nothing -> do
      let colors = judgeWordle a b
      atomically $ modifyTVar' var (M.insert pair colors)
      pure colors

insertColor :: [Color] -> M.Map [Color] Int -> M.Map [Color] Int
insertColor color = M.insertWith (+) color 1

countColors :: String -> [String] -> M.Map [Color] Int -> M.Map [Color] Int
countColors s xs = appEndo $ foldMap (Endo . insertColor . judgeWordle s) xs

countColorsDynamic :: String -> TVar (M.Map StringPair [Color]) -> [String] -> M.Map [Color] Int -> IO (M.Map [Color] Int)
countColorsDynamic s var xs m = do
  cs <- traverse (judgeWordleDynamic s var) xs
  let ms = foldMap (Endo . insertColor) cs
  pure $ appEndo ms m

wordleEntropyAverage :: String -> [String] -> Double
wordleEntropyAverage s dict_ = 
  let dict = filter (/= s) dict_
      sampleCount = length dict 
      maps = countColors s dict M.empty
  in M.foldr (\a b -> let p = fromIntegral a / fromIntegral sampleCount in p*negate (logBase 2 p) + b) 0 maps

wordleEntropyAverageDynamic :: String -> TVar (M.Map StringPair [Color]) -> [String] -> IO Double
wordleEntropyAverageDynamic s var dict_ =
  let dict = filter (/= s) dict_
      sampleCount = length dict 
      maps = countColorsDynamic s var dict M.empty
  in M.foldr (\a b -> let p = fromIntegral a / fromIntegral sampleCount in p*negate (logBase 2 p) + b) 0 <$> maps

calculateWordleEntropy :: String -> FilePath -> IO Double
calculateWordleEntropy s path = do
  file <- readFile path
  let ws = words file
  pure $ wordleEntropyAverage s ws

wordleEntropyRanking :: FilePath -> IO [(String, Double)]
wordleEntropyRanking path = do
  file <- readFile path
  let ws = words file
      f = id &&& flip wordleEntropyAverage ws
  pure $ LTH.sort (negate . snd) $ fmap f ws 

wordleEntropyRankingConcurrent :: FilePath -> IO [(String, Double)]
wordleEntropyRankingConcurrent path = do
  file <- readFile path
  let ws = words file
      f = id &&& flip wordleEntropyAverage ws
  LTH.sort (negate . snd) <$> mapConcurrently (pure . f) ws


-- dynamic version is very slow ...
-- Maybe, reading TVar is not efficient.  

wordleEntropyRankingDynamic :: FilePath -> IO [(String, Double)]
wordleEntropyRankingDynamic path = do
  file <- readFile path
  m <- newTVarIO M.empty 
  let ws = words file
      f = \x -> liftA2 (,) (pure x) (wordleEntropyAverageDynamic x m ws)
  LTH.sort (negate . snd) <$> mapConcurrently f ws