#!/usr/bin/env ruby

iterations = ARGV[0].to_i
step_dist = ARGV[1].to_i

Buffer = Struct.new(:len, :pos, :at1)

def step!(buffer, dist)
  next_val = buffer.len
  insert_after = (buffer.pos + dist) % buffer.len

  buffer.len = buffer.len + 1
  buffer.pos = insert_after + 1
  if insert_after == 0
    buffer.at1 = next_val
  end
end

def run(iterations, dist)
  b = Buffer.new(1, 0, 0)
  iterations.times { step!(b, dist) }
  b
end

final_b = run(iterations, step_dist)
puts final_b.inspect
