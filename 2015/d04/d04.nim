{.experimental: "parallel".}
from std/os import commandLineParams
from std/md5 import getMD5
from std/strformat import fmt
from std/strutils import startsWith, strip
import std/sequtils
import std/threadpool
import std/cpuinfo
import std/locks


type AsyncMiner = ref object
  key: string    # the private key from input
  prefix: string # target prefix of md5
  lock: Lock     # used for synchronizing threads
  memo: int      # the largest number tried.
  answer: int    # the smallest num found matching

const BLOCK_SIZE = 10_000

# get a block of numbers for a thread to work on
proc fetchNs(miner: AsyncMiner): seq[int] =
  withLock(miner.lock):
    let xs = (miner.memo + 1)..(miner.memo + BLOCK_SIZE)
    miner.memo = miner.memo + BLOCK_SIZE
    return toSeq(xs)

proc isDone(miner: AsyncMiner): bool =
  withLock(miner.lock):
    return miner.answer > 0

proc asyncWorker(miner: var AsyncMiner): void =
  while not miner.isDone():
    let xs = miner.fetchNs()
    for x in xs:
      if startsWith(getMD5(fmt"{miner.key}{x}"), miner.prefix):
        withLock(miner.lock):
          if miner.answer <= 0 or x < miner.answer:
            miner.answer = x
        return

proc mineAsync(key: string, prefix: string): int =
  var miner = AsyncMiner(key: key, prefix: prefix)
  parallel:
    for i in 0..countProcessors():
      spawn asyncWorker(miner)

  return miner.answer

proc main(): void =
  let inputFilename = commandLineParams()[0]
  let key = inputFilename.readFile().strip()

  echo "p1: ", mineAsync(key, "00000")
  echo "p2: ", mineAsync(key, "000000")

main()
