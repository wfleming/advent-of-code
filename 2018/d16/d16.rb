require "forwardable"
require "json"

Change = Struct.new(:before, :op, :after)

class Parser
  attr_reader :result

  def initialize(str)
    @lines = str.lines
    @result = {
      changes: [],
      instructions: [],
    }
  end

  def parse
    while @lines.any?
      parse_line(@lines.shift)
    end

    @result
  end

  def parse_line(line)
    if m = line.match(/Before: \[(\d+), (\d+), (\d+), (\d+)\]/)
      before = [m[1].to_i, m[2].to_i, m[3].to_i, m[4].to_i]
      op = @lines.shift.split(" ").map(&:to_i)
      m = @lines.shift.match(/After: +\[(\d+), (\d+), (\d+), (\d+)\]/)
      after = [m[1].to_i, m[2].to_i, m[3].to_i, m[4].to_i]
      @result[:changes] << Change.new(before, op, after)
    elsif line.match(/^\d/)
      op = line.split(" ").map(&:to_i)
      result[:instructions] << op
    end
  end
end

OP_CODE_NAMES = %i[
  addr
  addi
  mulr
  muli
  banr
  bani
  borr
  bori
  setr
  seti
  gtir
  gtri
  gtrr
  eqir
  eqri
  eqrr
].freeze

class OpCodeCandidates
  extend Forwardable

  attr_reader :change
  def_delegators :@change, :before, :after, :op

  def initialize(change)
    @change = change
  end

  def candidates
    OP_CODE_NAMES.map do |op_name|
      if self.send("#{op_name}?".to_sym)
        op_name
      end
    end.compact
  end

  def addr?
    after[op[3]] == before[op[1]] + before[op[2]]
  end

  def addi?
    after[op[3]] == before[op[1]] + op[2]
  end

  def mulr?
    after[op[3]] == before[op[1]] * before[op[2]]
  end

  def muli?
    after[op[3]] == before[op[1]] * op[2]
  end

  def banr?
    after[op[3]] == before[op[1]] & before[op[2]]
  end

  def bani?
    after[op[3]] == before[op[1]] & op[2]
  end

  def borr?
    after[op[3]] == before[op[1]] | before[op[2]]
  end

  def bori?
    after[op[3]] == before[op[1]] | op[2]
  end

  def setr?
    after[op[3]] == before[op[1]]
  end

  def seti?
    after[op[3]] == op[1]
  end

  def gtir?
    if op[1] > before[op[2]]
      after[op[3]] == 1
    else
      after[op[3]] == 0
    end
  end

  def gtri?
    if before[op[1]] > op[2]
      after[op[3]] == 1
    else
      after[op[3]] == 0
    end
  end

  def gtrr?
    if before[op[1]] > before[op[2]]
      after[op[3]] == 1
    else
      after[op[3]] == 0
    end
  end

  def eqir?
    if op[1] == before[op[2]]
      after[op[3]] == 1
    else
      after[op[3]] == 0
    end
  end

  def eqri?
    if before[op[1]] == op[2]
      after[op[3]] == 1
    else
      after[op[3]] == 0
    end
  end

  def eqrr?
    if before[op[1]] == before[op[2]]
      after[op[3]] == 1
    else
      after[op[3]] == 0
    end
  end
end

def at_least_three_candidates(changes)
  changes.map do |change|
    OpCodeCandidates.new(change).candidates
  end.select { |ops| ops.count >= 3 }
end

# hash of number => op_code
def identify_op_codes(changes)
  ops = {}

  until ops.count == OP_CODE_NAMES.count
    # only look at changes with unidentified opcodes
    changes_to_consider = changes.reject { |change| ops.key?(change.op[0]) }

    changes_to_consider.each do |change|
      candidates = OpCodeCandidates.new(change).candidates
      # prune possible matches that were already identified
      candidates.reject! { |candidate| ops.values.include?(candidate) }
      # store this identification if there's only 1 possibility
      if candidates.count == 1
        ops[change.op[0]] = candidates[0]
      end
    end
  end

  ops
end

class Machine
  attr_reader :op_map, :registers

  def initialize(op_map, instructions)
    @op_map = op_map
    @instructions = instructions.map(&:dup)
    @registers = [0, 0, 0, 0]
  end

  def run
    @instructions.each { |i| apply(i) }
  end

  def apply(instruction)
    send(op_map[instruction[0]], instruction[1], instruction[2], instruction[3])
  end

  def addr(a, b, c)
    registers[c] = registers[a] + registers[b]
  end

  def addi(a, b, c)
    registers[c] = registers[a] + b
  end

  def mulr(a, b, c)
    registers[c] = registers[a] * registers[b]
  end

  def muli(a, b, c)
    registers[c] = registers[a] * b
  end

  def banr(a, b, c)
    registers[c] = registers[a] & registers[b]
  end

  def bani(a, b, c)
    registers[c] = registers[a] & b
  end

  def borr(a, b, c)
    registers[c] = registers[a] | registers[b]
  end

  def bori(a, b, c)
    registers[c] = registers[a] | b
  end

  def setr(a, b, c)
    registers[c] = registers[a]
  end

  def seti(a, b, c)
    registers[c] = a
  end

  def gtir(a, b, c)
    registers[c] = (a > registers[b] ? 1 : 0)
  end

  def gtri(a, b, c)
    registers[c] = (registers[a] > b ? 1 : 0)
  end

  def gtrr(a, b, c)
    registers[c] = (registers[a] > registers[b] ? 1 : 0)
  end

  def eqir(a, b, c)
    registers[c] = (a == registers[b] ? 1 : 0)
  end

  def eqri(a, b, c)
    registers[c] = (registers[a] == b ? 1 : 0)
  end

  def eqrr(a, b, c)
    registers[c] = (registers[a] == registers[b] ? 1 : 0)
  end
end

if $0 == __FILE__
  input_str = File.read(ARGV[0])
  input = Parser.new(input_str).parse

  p1_r = at_least_three_candidates(input[:changes])
  puts "p1: of #{input[:changes].count} changes, #{p1_r.count} behave like 3 or more"

  puts "p2:"
  op_map = identify_op_codes(input[:changes])
  puts "op_map = #{JSON.pretty_generate(op_map)}"

  machine = Machine.new(op_map, input[:instructions])
  machine.run

  puts "p2: final registers are #{machine.registers}"
end
