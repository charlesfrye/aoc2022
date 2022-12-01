import * as fs from 'fs'
import * as path from 'path'

const inputPath = path.join(__dirname, process.argv[2])
const contents = fs.readFileSync(inputPath)

const elfStrings = contents.toString().split('\n\n')

const elfVectors = elfStrings.map(
  (string) => string.split('\n').map(
    (numberString) => parseInt(numberString)
  )
)

const elfTotals = elfVectors.map(
  (vector) => vector.reduce(
    (acc, next) => acc + next
  )
)

elfTotals.sort((a, b) => b - a)

const maxElves = elfTotals.slice(0, 3)

console.log(maxElves[0])

console.log(maxElves.reduce(
  (acc, next) => acc + next
))
