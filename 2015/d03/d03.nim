from std/os import commandLineParams
from std/sequtils import filter, toSeq
from std/strutils import strip
import std/strformat
import std/tables

type Pos = tuple[x: int, y: int] # x, y
type State = ref object
  pos: Pos
  visitedCounts: CountTableRef[Pos]

proc stepInstruction(state: State, instruction: char): void =
  # record the gift visit to current position
  state.visitedCounts.inc(state.pos)

  # advance the current position
  case instruction
  of '^': state.pos = (state.pos.x, state.pos.y + 1)
  of 'v': state.pos = (state.pos.x, state.pos.y - 1)
  of '>': state.pos = (state.pos.x + 1, state.pos.y)
  of '<': state.pos = (state.pos.x - 1, state.pos.y)
  else: raise newException(Exception, fmt"Unexpected instruction '{instruction}'")

proc runInstructions(state: State, instructions: string): void =
  for c in instructions:
    stepInstruction(state, c)

  # make sure last location is recorded visited
  state.visitedCounts.inc(state.pos)

proc runP2(instructions: string): (State, State) =
  let santa = State(pos: (0, 0), visitedCounts: newCountTable[Pos]())
  let roboSanta = State(pos: (0, 0), visitedCounts: newCountTable[Pos]())

  var curSanta = santa

  for c in instructions:
    stepInstruction(curSanta, c)
    curSanta = if curSanta == santa: roboSanta else: santa

  # record last locations as properly visited
  santa.visitedCounts.inc(santa.pos)
  roboSanta.visitedCounts.inc(roboSanta.pos)

  return (santa, roboSanta)

proc main(): void =
  let inputFilename = commandLineParams()[0]
  let instructions = inputFilename.readFile().strip()
  let state = State(pos: (0, 0), visitedCounts: newCountTable[Pos]())

  runInstructions(state, instructions)

  let p1_pos = toSeq(state.visitedCounts.keys).filter(proc(
      k: Pos): bool = return state.visitedCounts[k] > 1)

  echo "p1: houses visited at least once: ", state.visitedCounts.len()

  let (santa, roboSanta) = runP2(instructions)

  let p2AllVisits = newCountTable[Pos]()
  p2AllVisits.merge(santa.visitedCounts)
  p2AllVisits.merge(roboSanta.visitedCounts)
  echo "p2: houses visited at least once: ", p2AllVisits.len()

main()
