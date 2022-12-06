module Main (main) where

data Play = Rock | Paper | Scissors
  deriving (Eq, Show)

ties :: Play -> Play
ties = id

losesTo :: Play -> Play
losesTo Rock = Scissors
losesTo Paper = Rock
losesTo Scissors = Paper

beats :: Play -> Play
beats Rock = Paper
beats Paper = Scissors
beats Scissors = Rock

type Game = (Play, Play)

data Outcome = Lose | Draw | Win
  deriving (Eq, Show)

main :: IO ()
main = do
  contents <- getContents
  let finalScoreFirst = sum . map score $ gamesFirst
      finalScoreSecond = sum . map score $ gamesSecond
      gamesFirst = zip myPlaysFirst theirPlays
      gamesSecond = zip myPlaysSecond theirPlays
      myPlaysFirst = uncmap myFirstMap parsedContents
      myPlaysSecond = uncmap mySecondMap parsedContents
      theirPlays = uncmap theirMap parsedContents
      parsedContents = map parse $ lines contents
  print finalScoreFirst
  print finalScoreSecond

uncmap :: (a -> b -> c) -> [(a, b)] -> [c]
uncmap = map . uncurry

score :: Game -> Int
score (myPlay, theirPlay) = scorePlay myPlay + (scoreOutcome . getOutcome $ (myPlay, theirPlay))

scorePlay :: Play -> Int
scorePlay Rock = 1
scorePlay Paper = 2
scorePlay Scissors = 3

scoreOutcome :: Outcome -> Int
scoreOutcome Lose = 0
scoreOutcome Draw = 3
scoreOutcome Win = 6

getOutcome :: Game -> Outcome
getOutcome (myPlay, theirPlay)
  | myPlay == beats theirPlay = Win
  | myPlay == losesTo theirPlay = Lose
  | otherwise = Draw

myFirstMap :: Char -> Char -> Play
myFirstMap _ = playFromChar

mySecondMap :: Char -> Char -> Play
mySecondMap opp 'X' = losesTo . playFromChar $ opp
mySecondMap opp 'Y' = ties . playFromChar $ opp
mySecondMap opp 'Z' = beats . playFromChar $ opp
mySecondMap _ _ = undefined

theirMap :: Char -> Char -> Play
theirMap theirChar _ = playFromChar theirChar

parse :: String -> (Char, Char)
parse s = (head s, last s)

playFromChar :: Char -> Play
playFromChar 'A' = Rock
playFromChar 'B' = Paper
playFromChar 'C' = Scissors
playFromChar 'X' = Rock
playFromChar 'Y' = Paper
playFromChar 'Z' = Scissors
playFromChar _ = undefined