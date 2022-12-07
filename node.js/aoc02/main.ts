import * as fs from 'fs'
import * as path from 'path'

const inputPath = path.join(__dirname, process.argv[2])
const contents = fs.readFileSync(inputPath).toString()

enum RPS {
  Rock,
  Paper,
  Scissors
}

enum Outcome {
  Lose,
  Draw,
  Win
}

const theirPlayMap = new Map([
  ['A', RPS.Rock],
  ['B', RPS.Paper],
  ['C', RPS.Scissors]
]
)

const myPlayMap = new Map([
  ['X', RPS.Rock],
  ['Y', RPS.Paper],
  ['Z', RPS.Scissors]
]
)

const myOutcomeMap = new Map([
  ['X', Outcome.Lose],
  ['Y', Outcome.Draw],
  ['Z', Outcome.Win]
])

const outcomeScoreMap = new Map([
  [Outcome.Lose, 0],
  [Outcome.Draw, 3],
  [Outcome.Win, 6]
])

const playScoreMap = new Map([
  [RPS.Rock, 1],
  [RPS.Paper, 2],
  [RPS.Scissors, 3]
])

const playsFirstStyle = contents.split('\n').map(
  (line) => {
    const letters = line.split(' ')
    const [theirCommand, myCommand] = [letters[0], letters[1]]

    const theirPlay = theirPlayMap.get(theirCommand)
    const myPlay = myPlayMap.get(myCommand)

    return [myPlay, theirPlay] as [RPS, RPS] // extra type hint for ts compiler
  }
)
const playsSecondStyle = contents.split('\n').map(
  (line) => {
    const letters = line.split(' ')
    const [theirCommand, myCommand] = [letters[0], letters[1]]

    const theirPlay = theirPlayMap.get(theirCommand) ?? RPS.Rock
    const myOutcome = myOutcomeMap.get(myCommand) ?? Outcome.Draw
    const myPlay = getPlay(myOutcome, theirPlay)

    return [myPlay, theirPlay] as [RPS, RPS] // extra type hint for ts compiler
  }
)

function scorePlay (play: [RPS, RPS]): number {
  const outcome = getOutcome(play)
  const outcomeScore = outcomeScoreMap.get(outcome) ?? 0
  const playScore = playScoreMap.get(play[0]) ?? 0
  return outcomeScore + playScore
}

function getOutcome (play: [RPS, RPS]): Outcome {
  const [myPlay, theirPlay] = play
  if (myPlay === theirPlay) {
    return Outcome.Draw
  }
  if (myPlay === RPS.Rock && theirPlay === RPS.Scissors) {
    return Outcome.Win
  }
  if (myPlay === RPS.Paper && theirPlay === RPS.Rock) {
    return Outcome.Win
  }
  if (myPlay === RPS.Scissors && theirPlay === RPS.Paper) {
    return Outcome.Win
  }

  return Outcome.Lose
}

function getPlay (myOutcome: Outcome, theirPlay: RPS): RPS {
  if (myOutcome === Outcome.Win) {
    if (theirPlay === RPS.Rock) {
      return RPS.Paper
    } else if (theirPlay === RPS.Paper) {
      return RPS.Scissors
    } else if (theirPlay === RPS.Scissors) {
      return RPS.Rock
    }
  }
  if (myOutcome === Outcome.Lose) {
    if (theirPlay === RPS.Rock) {
      return RPS.Scissors
    } else if (theirPlay === RPS.Paper) {
      return RPS.Rock
    } else if (theirPlay === RPS.Scissors) {
      return RPS.Paper
    }
  }
  return theirPlay
}

const scoreFirstStyle = playsFirstStyle.map(scorePlay).reduce((a, b) => a + b, 0)
const scoreSecondStyle = playsSecondStyle.map(scorePlay).reduce((a, b) => a + b, 0)

console.log(scoreFirstStyle)
console.log(scoreSecondStyle)
