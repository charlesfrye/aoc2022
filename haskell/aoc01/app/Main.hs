module Main (main) where

import Text.Read (readMaybe)

main :: IO ()
main = do
  contents <- getContents
  let top3Elves = take 3 sortedElvesCalories
      sortedElvesCalories = qsort elvesCalories
      elvesCalories = total elvesSacks
      elvesSacks = parse contents
  print top3Elves
  print $ sum top3Elves

total :: [[Int]] -> [Int]
total = map sum

parse :: String -> [[Int]]
parse = splitSacks . lines

splitSacks :: [String] -> [[Int]]
splitSacks = splitOnNothing . readInts

readInts :: [String] -> [Maybe Int]
readInts = map (readMaybe :: String -> Maybe Int)

splitOnNothing :: [Maybe Int] -> [[Int]]
splitOnNothing = foldl buildSacks []
  where
    buildSacks :: [[Int]] -> Maybe Int -> [[Int]]
    buildSacks ls Nothing = [] : ls
    buildSacks [] (Just k) = [[k]]
    buildSacks (l : ls) (Just k) = (k : l) : ls

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) = biggerSorted ++ [x] ++ smallerSorted
  where
    smallerSorted = qsort [e | e <- xs, e <= x]
    biggerSorted = qsort [e | e <- xs, e > x]