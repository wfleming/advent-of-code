#!/usr/bin/env ruby

require "prime"

class VM
  def self.parse_instructions(file)
    File.readlines(file).map do |line|
      line.split(" ").map { |x| /^-?\d+$/.match?(x) ? x.to_i : x }
    end
  end

  attr_reader :registers, :ip

  def initialize(instructions)
    @instructions = instructions
    @ip = 0
    @registers = Hash.new(0)
  end

  def halted?
    @instructions[@ip].nil?
  end

  def lit_or_reg(x)
    x.is_a?(Numeric) ? x : registers[x]
  end

  def cur_instruction
    @instructions[@ip]
  end

  def step
    instr = cur_instruction
    ip_step = 1
    case instr[0]
    when "set"
      registers[instr[1]] = lit_or_reg(instr[2])
    when "sub"
      registers[instr[1]] -= lit_or_reg(instr[2])
    when "mul"
      registers[instr[1]] *= lit_or_reg(instr[2])
    when "jnz"
      ip_step = lit_or_reg(instr[2]) unless lit_or_reg(instr[1]).zero?
    end
    @ip += ip_step
  end

  def snd(x)
    @snd_freq = lit_or_reg(x)
  end

  def rcv(x)
    @recv_freq = @snd_freq if x != registers[x]
    true
  end
end

def p2
  # I glanced at reddit posts, I think the step size of 17 is static for all
  # inputs, and only the values calculating b & c are variable, but this pulls
  # it from the input anyway so this function should work for all inputs, I
  # think.
  vm = VM.new(INSTRUCTIONS)
  vm.registers["a"] = 1
  8.times { vm.step }
  b = vm.registers["b"]
  c = vm.registers["c"]
  step = INSTRUCTIONS[-2][2].abs

  # outer loop lines 9 - 32 iterates from b to c, incrementing by 17 each time
  # it does this by mutating b, but let's say x = c - b.
  # lines 9-24 is a loop that tests if x is divisible by each number in 2...x,
  # sets f = 0 if it is, and breaks out ("jnz g -8" stop looping)
  # h increments when f = 0, which is when x has any divisor (is not prime)
  # so the overall program logic is "count numbers in range b..c, incrementing
  # by 17, that are not prime"
  (b..c).step(17).count { |x| !x.prime? }
end

if __FILE__ == $0
  INSTRUCTIONS = VM.parse_instructions(ARGV[0])
  vm1 = VM.new(INSTRUCTIONS)
  mul_count = 0
  until vm1.halted?
    mul_count += 1 if vm1.cur_instruction&.[](0) == "mul"
    vm1.step
  end
  puts "p1: mul was executed #{mul_count} times"

  puts "p2: register h=#{p2}"
end
