from std/os import commandLineParams
from std/sequtils import map
from std/strutils import parseInt, split, strip
from math import sum


type Box = tuple[l: int, w: int, h: int]

func parseBox(box: string): Box =
  let pieces = box.split("x").map(parseInt)
  return (pieces[0], pieces[1], pieces[2])

func area(box: Box): int =
  return 2*box.l*box.w + 2*box.w*box.h + 2*box.h*box.l

func paperNeeded(box: Box): int =
  return box.area() + [box.l * box.w, box.w * box.h, box.h * box.l].min()

func ribbonNeeded(box: Box): int =
  let faces = [
    2 * (box.l + box.w),
    2 * (box.w + box.h),
    2 * (box.h + box.l),
  ]
  let bow = box.w * box.l * box.h
  return faces.min() + bow

proc main(): void =
  let inputFilename = commandLineParams()[0]
  let inputLines = inputFilename.readFile().strip().split("\n")
  let boxes = inputLines.map(parseBox)

  echo "p1: total sq ft paper needed ", boxes.map(paperNeeded).sum()
  echo "p2: total ft ribbon needed ", boxes.map(ribbonNeeded).sum()

main()
