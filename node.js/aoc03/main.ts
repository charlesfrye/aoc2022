import * as fs from 'fs'
import * as path from 'path'

function main (): void {
  const inputPath = path.join(__dirname, process.argv[2])
  const contents = fs.readFileSync(inputPath).toString()
  const rucksacks = getRucksacks(contents)
  // part 1
  const compartments = getCompartments(rucksacks)
  const uniqueItems = getUniquesSackwise(compartments)
  const sharedItems = getIntersectionSackwise(uniqueItems)
  const itemPriorities = getPriorities(sharedItems)
  const totalPriority = itemPriorities.reduce((acc, nxt) => acc + nxt, 0)
  console.log(totalPriority)

  // part 2
  const groups = groupRucksacks(rucksacks)
  const groupUniques = groups.map(grp => grp.map(string => new Set(string)))
  const badges = getIntersectionGroupwise(groupUniques)
  const badgePriorities = getPriorities(badges)
  const totalBadgePriority = badgePriorities.reduce((acc, nxt) => acc + nxt, 0)
  console.log(totalBadgePriority)
}

function getPriorities (items: string[]): number[] {
  return items.map(getPriority)
}

function getPriority (item: string): number {
  return (item.charCodeAt(0) - 38) % 58
}

function groupRucksacks<T> (rucksacks: T[]): T[][] {
  const groups: T[][] = [[]]
  for (const rucksack of rucksacks) {
    if (groups.slice(-1)[0].length >= 3) {
      groups.push([])
    }
    const currentGroup = groups.slice(-1)[0]
    currentGroup.push(rucksack)
  }
  return groups
}

function getIntersectionSackwise (uniqueItems: Array<Array<Set<string>>>): string[] {
  return uniqueItems.map(getIntersection)
}

function getIntersectionGroupwise (groups: Array<Array<Set<string>>>): string[] {
  return groups.map(getIntersection)
}

function getIntersection (sets: Array<Set<string>>): string {
  return sets.reduce((acc, nxt) => acc.filter(item => nxt.has(item)), [...sets[0]])[0]
}

function getUniquesSackwise (compartmentsSackwise: string[][]): Array<Array<Set<string>>> {
  return compartmentsSackwise.map(sack => [getUniques(sack[0]), getUniques(sack[1])])
}

function getUniques (string: string): Set<string> {
  return new Set(string)
}

function getCompartments (rucksacks: string[]): string[][] {
  const compartments = rucksacks.map(sack => [sack.slice(0, sack.length / 2), sack.slice(sack.length / 2, sack.length)])

  return compartments
}

function getRucksacks (string: string): string[] {
  return string.split('\n')
}

main()
