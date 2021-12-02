#!/usr/bin/env ruby

COMMANDS = File.readlines(ARGV[0]).map do |line|
  pieces = line.split(" ")
  [pieces[0].to_sym, Integer(pieces[1])]
end

# coords are [horizontal, depth]
def p1_apply(pos, cmd)
  case cmd[0]
  when :down
    [pos[0], pos[1] + cmd[1]]
  when :up
    [pos[0], pos[1] - cmd[1]]
  when :forward
    [pos[0] + cmd[1], pos[1]]
  else
    raise ArgumentError, "unexpected command direction in #{cmd}"
  end
end

# coords are [horizontal, depth, aim]
def p2_apply(pos, cmd)
  case cmd[0]
  when :down
    [pos[0], pos[1], pos[2] + cmd[1]]
  when :up
    [pos[0], pos[1], pos[2] - cmd[1]]
  when :forward
    [pos[0] + cmd[1], pos[1] + (pos[2] * cmd[1]), pos[2]]
  else
    raise ArgumentError, "unexpected command direction in #{cmd}"
  end
end

def run(start, cmds, apply_fn)
  cmds.inject(start, &apply_fn)
end

p1_end = run([0, 0], COMMANDS, method(:p1_apply))
puts "p1: ends at #{p1_end}, answer = #{p1_end[0] * p1_end[1]}"

p2_end = run([0, 0, 0], COMMANDS, method(:p2_apply))
puts "p2: ends at #{p2_end}, answer = #{p2_end[0] * p2_end[1]}"
