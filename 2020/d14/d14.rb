#!/usr/bin/env ruby

INSTRUCTIONS = File.readlines(ARGV[0]).map(&:freeze).freeze

class Program
  attr_reader :memory, :mask

  MASK_PAT = /^mask = (?<mask>[10X]+)/
  MEM_PAT = /^mem\[(?<addr>\d+)\] = (?<val>\d+)/

  def initialize
    @memory = {}
    @mask = nil
  end

  def mem_sum
    memory.values.sum
  end

  def run_program(instructions)
    instructions.each(&method(:run_instruction))
  end

  def run_instruction(instruction)
    if (m = MASK_PAT.match(instruction))
      @mask = m["mask"]
    elsif (m = MEM_PAT.match(instruction))
      memory[Integer(m["addr"])] = apply_mask(Integer(m["val"]))
    else
      raise "couldn't parse '#{instruction}'"
    end
  end

  def apply_mask(n)
    raise "should have a mask set before setting mem" if mask.nil?
    n_bin = "%0#{mask.length}b" % n

    mask.each_char.each_with_index do |char, idx|
      if %w[1 0].include?(char)
        n_bin[idx] = char
      end
    end

    Integer(n_bin, 2)
  end
end

# p1
p = Program.new
p.run_program(INSTRUCTIONS)
puts "p1: sum of memory is #{p.mem_sum}"
