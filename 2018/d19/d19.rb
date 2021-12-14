require "d16"

class Machine19 < Machine
  attr_reader :instructions, :ip_reg, :ip

  def self.parse(str)
    instrs = str.lines.map do |l|
      pieces = l.split
      [pieces.shift] + pieces.map(&:to_i)
    end
    ip_reg = instrs.shift[1]
    new(instrs, ip_reg)
  end

  def initialize(instructions, ip_reg)
    op_map = Hash.new do |_, k|
      if k == "#ip"
        :ip
      else
        k
      end
    end
    super(op_map, instructions)
    @registers = [0, 0, 0, 0, 0, 0]
    @ip_reg = ip_reg
    @ip = 0
  end

  def run
    tick while @instructions[ip]
  end

  def tick
    apply(@instructions[ip])
  end

  def apply(op)
    registers[ip_reg] = ip
    instr = @instructions[ip]
    super
    @ip = registers[ip_reg] + 1
  end
end

def p2(r0)
  # program starts by jumping to ip=17 (addi 3 2 3)
  # ip 17-24 initialize r3 to a large number (larger when r0 = 1)
  # then jump back to ip = 1
  # then ip 1-16 loop (r1, r2) through all vals 0..(r3 + 1)
  # and for each pair, increment r0 by r2 when r1 * r2 == r3
  # "sum of all divisors of r3"

  # Could transcribe the math here for calculating r3, but this way I can
  # support other inputs
  m = Machine19.parse(File.read(ARGV[0]))
  m.registers[0] = r0
  m.tick until m.ip == 1
  iterations = m.registers[3]

  (1..iterations).select { |x| iterations % x == 0 }.sum
end

if $0 == __FILE__
  m = Machine19.parse(File.read(ARGV[0]))
  m.run
  puts "p1: registers after halt are #{m.registers}"
  puts "p1 with p2 impl: after halting r0 = #{p2(0)}"

  puts "p2: after halting r0 = #{p2(1)}"
end
