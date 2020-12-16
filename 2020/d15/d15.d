// `dmd -O d15.d && ./d15 input.txt` to run
import std.array : split, array;
import std.algorithm : map;
import std.string : strip;
import std.conv : parse;
import std.file : readText;
import std.stdio;

struct Game
{
  long lastTurn, lastSaid, sayNext;
  long[long] ages;

  this(long[] seed)
  {
    foreach(idx, val; seed)
    {
      this.ages[val] = idx + 1;
    }
    this.lastSaid = seed[seed.length - 1];
    this.sayNext = 0;
    this.lastTurn = seed.length;
  }

  private void playTurn()
  {
    lastSaid = sayNext;
    lastTurn += 1;
    auto lastSaidAtTurn = (sayNext in ages);
    if (lastSaidAtTurn is null) {
      sayNext = 0;
    } else {
      sayNext = lastTurn - *lastSaidAtTurn;
    }
    ages[lastSaid] = lastTurn;
  }

  public void playToTurn(int turnCount)
  {
    while (this.lastTurn < turnCount)
    {
      this.playTurn();
    }
  }
}

long[] parseInput(string path)
{
  string txt = readText(path);
  // parse!(long(x.strip())) doesn't parse for some reason?
  return txt.split(",").map!(x => parse!long(x)).array;
}

void main(string[] args)
{
  auto seed = parseInput(args[1]);
  auto g = new Game(seed);

  // p1
  g.playToTurn(2020);
  writeln("p1: after ", g.lastTurn, " turns, last number spoken was ", g.lastSaid);

  // p2
  g.playToTurn(30_000_000);
  writeln("p2: after ", g.lastTurn, " turns, last number spoken was ", g.lastSaid);
}
