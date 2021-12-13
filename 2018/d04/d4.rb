require "time"

WAKE = "wakes up".freeze
SLEEP = "falls asleep".freeze

class Entry
  attr_reader :guard, :time_asleep, :time_awake

  def initialize(guard, time_asleep, time_awake)
    @guard = guard
    @time_asleep = time_asleep
    @time_awake = time_awake
  end

  def ==(other)
    other.guard == guard &&
        other.time_asleep == time_asleep &&
        other.time_awake == time_awake
  end

  def minutes_asleep
    (time_asleep.min..(time_awake.min - 1)).to_a
  end
end

class Parser
  LINE_PAT = /\[(.+)\] (.+)/
  GUARD_PAT = /Guard #(\d+) begins shift/

  def parse(lines)
    pieces = lines.map do |line|
      m = LINE_PAT.match(line)
      action = nil
      if (m2 = m[2].match(GUARD_PAT))
        action = m2[1].to_i
      elsif m[2] == WAKE || m[2] == SLEEP
        action = m[2]
      else
        raise "unexpected bit of line: #{m[2]}"
      end
      [Time.parse(m[1]), action]
    end.sort_by { |l| l[0] }

    gid = nil
    time_asleep = nil
    entries = []
    pieces.each do |piece|
      if piece[1].is_a?(Integer)
        gid = piece[1]
      elsif piece[1] == SLEEP
        time_asleep = piece[0]
      elsif piece[1] == WAKE
        entries << Entry.new(gid, time_asleep, piece[0])
        time_asleep = nil
      end
    end

    entries
  end
end

class Calc
  attr_reader :entries

  def initialize(entries)
    @entries = entries
  end

  def guard_sleeps
    @guard_sleeps ||= {}.tap do |h|
      entries.each do |entry|
        unless h.key?(entry.guard)
          h[entry.guard] = [0] * 60
        end
        entry.minutes_asleep.each do |minute|
          h[entry.guard][minute] += 1
        end
      end
    end
  end

  def sleepiest_guard
    max_mins = guard_sleeps.values.map(&:sum).max

    guard_sleeps.find { |_, mins| mins.sum == max_mins }.first
  end

  def consistent_guard
    guard = -1
    sleepiest_min_count = -1

    guard_sleeps.each do |g, v|
      if v.max > sleepiest_min_count
        guard = g
        sleepiest_min_count = v.max
      end
    end

    guard
  end

  def guard_tot_sleep(guard)
    guard_sleeps[guard].sum
  end

  def guard_sleepiest_min(guard)
    sleepiest_count = guard_sleeps[guard].max
    guard_sleeps[guard].each_with_index.find { |min, idx| min == sleepiest_count }[1]
  end
end

if $0 == __FILE__
  input = File.read(ARGV[0]).lines
  entries = Parser.new.parse(input)

  calc = Calc.new(entries)

  g = calc.sleepiest_guard
  tot_sleep = calc.guard_tot_sleep(g)
  sleepiest_min = calc.guard_sleepiest_min(g)

  puts "p1: sleepiest guard is #{g}, slept for #{tot_sleep} mins, sleepiest at #{sleepiest_min}, answer is #{g * sleepiest_min}"

  g = calc.consistent_guard
  sleepiest_min = calc.guard_sleepiest_min(g)

  puts "p2: sleepiest guard is #{g}, sleepiest at #{sleepiest_min}, answer is #{g * sleepiest_min}"
end
