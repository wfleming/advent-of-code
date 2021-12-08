#!/usr/bin/env ruby

class VM
  def self.parse(file)
    new(File.readlines(file).map(&:split))
  end

  attr_reader :registers

  def initialize(instructions)
    @instructions = instructions
    @ip = 0
    @registers = {}
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
    when "tgl"
      x = @ip + reg_or_lit(instr[1])
      if @instructions[x].nil?
        #nop
      elsif @instructions[x][0] == "inc"
        @instructions[x][0] = "dec"
      elsif @instructions[x].count == 2
        @instructions[x][0] = "inc"
      elsif @instructions[x][0] == "jnz"
        @instructions[x][0] = "cpy"
      else
        @instructions[x][0] = "jnz"
      end
    when "mlt"
      registers[instr[1]] *= reg_or_lit(instr[2])
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

  def optimize!
    @instructions = Optimizer.new(@instructions.clone).optimized
  end
end

vm = VM.parse(ARGV[0])
vm.registers["a"] = 7
vm.run
puts "p1: after running with input 7, register a contains #{vm.registers["a"]}"

# run the p2 vm with p1 input as a sanity check
vm = VM.parse(ARGV[1])
vm.registers["a"] = 7
vm.run
puts "p2 sanity check: with the p1 input, register a is #{vm.registers["a"]}"

vm = VM.parse(ARGV[1])
vm.registers["a"] = 12
vm.run
puts "p2: after running with input 12, register a contains #{vm.registers["a"]}"
