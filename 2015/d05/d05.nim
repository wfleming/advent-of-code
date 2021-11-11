from std/os import commandLineParams
import std/strutils
import std/sequtils
import std/enumerate
import std/tables

const
  VOWELS = ['a', 'e', 'i', 'o', 'u']
  NAUGHTY_PAIRS = ["ab", "cd", "pq", "xy"]

func vowelCount(s: string): int =
  return toSeq(s).filter(func (c: char): bool = return c in VOWELS).len()

func hasDoubleLetter(s: string): bool =
  for i, c in enumerate(s[0..(s.len() - 2)]):
    if c == s[i + 1]:
      return true

  return false

func hasNaughtyPair(s: string): bool =
  return any(NAUGHTY_PAIRS, func (p: string): bool = return p in s)

func isNice(s: string): bool =
  return vowelCount(s) >= 3 and
    hasDoubleLetter(s) and
    not hasNaughtyPair(s)

func hasRepeatedPair(s: string): bool =
  var firstPairs = initTable[string, int]()
  for i, c in enumerate(s[0..(s.len() - 2)]):
    let pair = c & s[i + 1]
    if pair in firstPairs:
      if i > firstPairs[pair] + 1:
        return true
    else:
      firstPairs[pair] = i

  return false

func hasSeparatedPair(s: string): bool =
  for i, c in enumerate(s[0..(s.len() - 3)]):
    if c == s[i + 2]:
      return true

  return false

func isNice2(s: string): bool =
  return hasRepeatedPair(s) and hasSeparatedPair(s)

proc main(): void =
  let inputFilename = commandLineParams()[0]
  let strs = toSeq(inputFilename.lines)

  echo "total of ", strs.len(), " strings to check"

  let niceStrings = strs.filter(isNice)
  echo "p1: there are ", niceStrings.len(), " nice strings"

  let niceStrings2 = strs.filter(isNice2)
  echo "p2: there are ", niceStrings2.len(), " nice strings"

main()
