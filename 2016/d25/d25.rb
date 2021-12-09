#!/usr/bin/env ruby

class VM
  def self.parse(file)
    new(File.readlines(file).map(&:split))
  end

  attr_reader :registers, :output

  def initialize(instructions)
    @instructions = instructions
    @ip = 0
    @registers = {}
    @output = []
  end

  def reg_or_lit(x)
    if x =~ /^-?\d+$/
      x.to_i
    else
      registers[x]
    end
  end

  def step
    instr = @instructions[@ip]
    ip_step = 1
    case instr[0]
    when "cpy"
      registers[instr[2]] = reg_or_lit(instr[1])
    when "inc"
      registers[instr[1]] += 1
    when "dec"
      registers[instr[1]] -= 1
    when "jnz"
      ip_step = reg_or_lit(instr[2]) if reg_or_lit(instr[1]) != 0
    when "out"
      output << reg_or_lit(instr[1])
    else
      raise StandardError, "don't know how to handle instruction #{instr}"
    end
    @ip += ip_step
  end

  def halted?
    @ip < 0 || @ip >= @instructions.count
  end

  def run
    step while !halted?
  end

  def run_until_output_full(n)
    step until halted? || output.count == n
  end
end

a = 0
found = false
needed_signal = ([0, 1] * 5)
until found
  a += 1
  # puts "testing a=#{a}"
  vm = VM.parse(ARGV[0])
  vm.registers["a"] = a
  vm.run_until_output_full(needed_signal.count)
  found = (needed_signal == vm.output)
end

puts "register a needs #{a} to output clock signal"
