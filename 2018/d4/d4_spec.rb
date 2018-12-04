require "minitest/autorun"

require "d4"

describe Entry do
  describe "#minutes_asleep" do
    it "is correct" do
      e = Entry.new(10, Time.parse("1518-11-01 00:05"), Time.parse("1518-11-01 00:25"))

      e.minutes_asleep.must_equal (5..24).to_a
    end
  end
end

describe Parser do
  it "parses some lines" do
    input = <<~LINES
    [1518-11-01 00:00] Guard #10 begins shift
    [1518-11-01 00:05] falls asleep
    [1518-11-01 00:25] wakes up
    [1518-11-01 00:30] falls asleep
    [1518-11-01 00:55] wakes up
    [1518-11-02 00:40] falls asleep
    [1518-11-02 00:50] wakes up
    [1518-11-01 23:58] Guard #99 begins shift
    LINES
    # last line is out of order

    entries = Parser.new.parse(input.lines)
    entries.must_equal [
      Entry.new(10, Time.parse("1518-11-01 00:05"), Time.parse("1518-11-01 00:25")),
      Entry.new(10, Time.parse("1518-11-01 00:30"), Time.parse("1518-11-01 00:55")),
      Entry.new(99, Time.parse("1518-11-02 00:40"), Time.parse("1518-11-02 00:50")),
    ]
  end
end

SAMPLE_INPUT = <<~LINES
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up
LINES

describe Calc do
  it "works for calc" do
    entries = Parser.new.parse(SAMPLE_INPUT.lines)
    calc = Calc.new(entries)

    g = calc.sleepiest_guard
    tot_sleep = calc.guard_tot_sleep(g)
    sleepiest_min = calc.guard_sleepiest_min(g)

    g.must_equal 10
    tot_sleep.must_equal 50
    sleepiest_min.must_equal 24
  end

  it "works for p2" do
    entries = Parser.new.parse(SAMPLE_INPUT.lines)
    calc = Calc.new(entries)

    g = calc.consistent_guard
    sleepiest_min = calc.guard_sleepiest_min(g)

    g.must_equal 99
    sleepiest_min.must_equal 45
  end
end
