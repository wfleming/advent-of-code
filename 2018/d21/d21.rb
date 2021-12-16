#!/usr/bin/env -S ruby -I../d19/ -I../d16/

require "d19"
require "set"

class Machine21 < Machine19
  attr_reader :tick_count

  def initialize(instructions, ip_reg)
    super
    @tick_count = 0
  end

  def tick
    @tick_count += 1
    super
  end
end

def find_longest_running_r0
  r2_seen = Set.new

  prev_r2 = 0
  r2 = 0

  loop do
    r5 = r2 | 65536 # ip 6
    r2 = 4843319    # ip 7

    loop do
      r4 = r5 & 255 # ip 8
      r2 = (((r2 + r4) & 16777215) * 65899) & 16777215 # ip 9 - 12

      break if 256 > r5 # ip 13 condition

      # this calc is done via a loop starting at ip 17, then looping back to 18 from
      # 25. It starts with r4 = 0, and increments it until ((r4 + 1) * 256) > r5
      # seems like just a very long way of doing r5 = r5 / 256
      r4 = (r5 / 256)
      r5 = r4
      # return to ip 8
    end

    # ip 28, kinda: store what we're seeing for r2
    if r2_seen.include?(r2) # saw the same value, we're looping
      # if we're looping to an already seen r2, that means the *previous* r2
      # was the longest runtime to get there first, which satisfies the
      # confusing definition "the lowest non-negative integer value for register
      # 0 that causes the program to halt after executing the most instructions"
      return prev_r2
    else
      r2_seen << r2
      prev_r2 = r2
    end
    # back to ip 6
  end
end

if $0 == __FILE__
  m = Machine21.parse(File.read(ARGV[0]))
  m.tick until m.ip == 28
  puts "p1: at the if condition to terminate when r2 == r0, r2=#{m.registers[2]} (took #{m.tick_count} ticks)"

  puts "p2: #{find_longest_running_r0}"
end
