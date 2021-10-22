import std/os
import std/enumerate
import std/strformat
import std/strutils


type State = ref object
  floor: int
  enterBasementPos: int


proc run(state: var State, inputFilename: string) =
  # inputFilename.readFile().strip() would also work
  var input = strip(readFile(inputFilename))
  for i, c in enumerate(1, input):
    case c
    of '(': state.floor += 1
    of ')':
      state.floor -= 1
      if state.floor == -1 and state.enterBasementPos < 1:
        state.enterBasementPos = i
    else: raise newException(Exception, fmt"Unexpected char '{c}'")


proc main(): void =
  let inputFilename = commandLineParams()[0]
  var state = State(floor: 0, enterBasementPos: -1)
  state.run(inputFilename)
  echo "p1: Santa is on floor ", state.floor
  echo "p2: First position to enter basement ", state.enterBasementPos


main()
