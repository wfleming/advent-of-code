from std/os import commandLineParams
from std/md5 import getMD5
from std/strformat import fmt
from std/strutils import startsWith, strip

proc mine(key: string, prefix: string): int =
  var x = 1
  while not startsWith(getMD5(fmt"{key}{x}"), prefix):
    x += 1

  return x

proc main(): void =
  let inputFilename = commandLineParams()[0]
  let key = inputFilename.readFile().strip()

  echo "p1: ", mine(key, "00000")
  echo "p2: ", mine(key, "000000")

main()
