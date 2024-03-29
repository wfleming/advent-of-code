from std/os import commandLineParams
import std/re
import std/sequtils
import std/strformat
import std/strutils

type
  LightGrid[T] = array[1_000, array[1_000, T]]
  Action = enum on, off, toggle
  Pos = tuple[x: int, y: int]
  Command = object
    action: Action
    fromPos: Pos
    toPos: Pos

proc parseCommand(s: string): Command =
  let pat = re"(toggle|turn off|turn on) (\d+),(\d+) through (\d+),(\d+)"
  var groups: array[5, string]
  let i = find(s, pat, groups)
  if i >= 0:
    var a = case groups[0]
      of "toggle": Action.toggle
      of "turn off": Action.off
      of "turn on": Action.on
      else: raise newException(Exception, "Invalid action")
    return Command(
      action: a,
      fromPos: (parseInt(groups[1]), parseInt(groups[2])),
      toPos: (parseInt(groups[3]), parseInt(groups[4])),
    )
  else:
    raise newException(Exception, fmt"Couldn't parse command '{s}'")

proc applyCommand(grid: var LightGrid[bool], cmd: Command): void =
  for x in cmd.fromPos.x..cmd.toPos.x:
    for y in cmd.fromPos.y..cmd.toPos.y:
      case cmd.action
      of Action.toggle: grid[x][y] = not grid[x][y]
      of Action.on: grid[x][y] = true
      of Action.off: grid[x][y] = false

proc applyCommand(grid: var LightGrid[uint], cmd: Command): void =
  for x in cmd.fromPos.x..cmd.toPos.x:
    for y in cmd.fromPos.y..cmd.toPos.y:
      case cmd.action
      of Action.toggle: grid[x][y] = grid[x][y] + 2
      of Action.on: grid[x][y] = grid[x][y] + 1
      of Action.off: grid[x][y] = grid[x][y] - min(grid[x][y], 1)

proc applyAll[T](grid: var LightGrid[T], cmds: seq[Command]): void =
  for c in cmds:
    grid.applyCommand(c)

func litCount(grid: LightGrid[bool]): int =
  result = 0
  for x in 0..999:
    for y in 0..999:
      if grid[x][y]:
        result += 1
  return result

func totalBrightness(grid: LightGrid[uint]): uint =
  result = 0
  for x in 0..999:
    for y in 0..999:
      result += grid[x][y]
  return result

proc main(): void =
  let inputFilename = commandLineParams()[0]
  let lines = toSeq(inputFilename.lines)
  let commands = lines.map(parseCommand)

  var grid: LightGrid[bool]
  grid.applyAll(commands)
  echo "p1: there are ", grid.litCount(), " lights on"

  var grid2: LightGrid[uint]
  grid2.applyAll(commands)
  echo "p2: total brightness is ", grid2.totalBrightness()

main()
