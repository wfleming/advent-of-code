#!/usr/bin/env ruby

class Program
  def self.parse(str)
    instructions = str.lines.map { |l| l.chomp.split(" ").map { |token| token.chomp(",") } }
    new(instructions)
  end

  attr_reader :registers, :ip

  def initialize(instructions)
    @instructions = instructions
    @registers = { "a" => 0, "b" => 0 }
    @ip = 0
  end

  def halted?
    ip < 0 || ip >= @instructions.count
  end

  def run
    step until halted?
  end

  def step
    instruction = @instructions[ip]

    case instruction[0]
    when "hlf"
      @registers[instruction[1]] /= 2
      @ip += 1
    when "tpl"
      @registers[instruction[1]] *= 3
      @ip += 1
    when "inc"
      @registers[instruction[1]] += 1
      @ip += 1
    when "jmp"
      @ip += Integer(instruction[1])
    when "jie"
      if @registers[instruction[1]] % 2 == 0
        @ip += Integer(instruction[2])
      else
        @ip += 1
      end
    when "jio"
      if @registers[instruction[1]] == 1
        @ip += Integer(instruction[2])
      else
        @ip += 1
      end
    else
      raise StandardError, "Invalid instruction #{instruction.inspect}"
    end
  end
end

prog = Program.parse(File.read(ARGV[0]))
prog.run
puts "p1: final registers are #{prog.registers}"

prog = Program.parse(File.read(ARGV[0]))
prog.registers["a"] = 1
prog.run
puts "p2: final registers are #{prog.registers}"
