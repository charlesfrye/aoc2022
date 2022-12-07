module Main (main) where

import Data.Char (ord)
import Data.Set (Set, elems, fromList, intersection)

type Compartment = [Char]

type SplitRucksack = [Compartment]

type Rucksack = [Char]

allowedChars :: Set Char
allowedChars = fromList $ ['A' .. 'Z'] ++ ['a' .. 'z']

main :: IO ()
main = do
  contents <- getContents
  let sharedTotalPriority = sum sharedPriorities
      sharedPriorities = map toPriority sharedItems
      sharedItems = map getSharedItem splitRucksacks
      splitRucksacks = map split rucksacks
      rucksacks = parseRucksacks contents
  print sharedTotalPriority
  let badgesTotalPriority = sum badgePriorities
      badgePriorities = map toPriority badges
      badges = map getSharedItem groups
      groups = intoGroups rucksacks
  print badgesTotalPriority

getSharedItem :: [[Char]] -> Char
getSharedItem tas = head . elems $ intersectedSets
  where
    intersectedSets = foldl intersection allowedChars asSets
    asSets = fmap fromList tas

split :: Rucksack -> SplitRucksack
split r = pairToList $ splitAt ((length r + 1) `div` 2) r

pairToList :: (a, a) -> [a]
pairToList (x, y) = [x, y]

parseRucksacks :: String -> [String]
parseRucksacks = lines

intoGroups :: [Rucksack] -> [[Rucksack]]
intoGroups = foldl groupAcc []

groupAcc :: [[Rucksack]] -> Rucksack -> [[Rucksack]]
groupAcc rss new
  | null rss = [[new]]
  | length (head rss) >= 3 = [new] : rss
  | otherwise = (new : head rss) : tail rss

toPriority :: Char -> Int
toPriority c = (ord c - 38) `mod` 58