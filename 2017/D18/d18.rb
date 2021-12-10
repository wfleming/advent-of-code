#!/usr/bin/env ruby

class VM
  def self.parse_instructions(file)
    File.readlines(file).map do |line|
      line.split(" ").map { |x| /^-?\d+$/.match?(x) ? x.to_i : x }
    end
  end

  attr_reader :registers, :recv_freq

  def initialize(instructions)
    @instructions = instructions
    @ip = 0
    @registers = Hash.new(0)
    @recv_freq = nil
    @snd_freq = nil
  end

  def halted?
    @instructions[@ip].nil?
  end

  def lit_or_reg(x)
    x.is_a?(Numeric) ? x : registers[x]
  end

  def step
    instr = @instructions[@ip]
    ip_step = 1
    case instr[0]
    when "set"
      registers[instr[1]] = lit_or_reg(instr[2])
    when "add"
      registers[instr[1]] += lit_or_reg(instr[2])
    when "mul"
      registers[instr[1]] *= lit_or_reg(instr[2])
    when "mod"
      registers[instr[1]] %= lit_or_reg(instr[2])
    when "jgz"
      ip_step = lit_or_reg(instr[2]) if lit_or_reg(instr[1]) > 0
    when "snd"
      snd(instr[1])
    when "rcv"
      ip_step = 0 unless rcv(instr[1])
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

class VM2 < VM
  attr_reader :snd_count

  def initialize(instructions, id:, in_queue: , out_queue:)
    super(instructions)
    @in_queue = in_queue
    @out_queue = out_queue
    @terminated = false
    @snd_count = 0
    registers["p"] = id
  end

  def terminate!
    @terminated = true
  end

  def halted?
    super || @terminated
  end

  def blocked?
    !halted? && @instructions[@ip][0] == "rcv" && @in_queue.empty?
  end

  def snd(x)
    @snd_count += 1
    @out_queue.push(lit_or_reg(x))
  end

  def rcv(x)
    if @in_queue.empty?
      sleep 0.05 # nop, but sleep a bit to avoid thrashing
      false
    else
      # can't block here - if we block, we can't terminate
      registers[x] = @in_queue.pop(true)
      true
    end
  end
end

INSTRUCTIONS = VM.parse_instructions(ARGV[0])
vm1 = VM.new(INSTRUCTIONS)
vm1.step while vm1.recv_freq.nil? && !vm1.halted?
puts "p1: the freq is #{vm1.recv_freq}"

pipe0, pipe1 = Queue.new, Queue.new
vm0 = VM2.new(INSTRUCTIONS, id: 0, in_queue: pipe0, out_queue: pipe1)
vm1 = VM2.new(INSTRUCTIONS, id: 1, in_queue: pipe1, out_queue: pipe0)
[
  Thread.new { vm0.step until vm0.halted? || (vm0.blocked? && (vm1.blocked? || vm1.halted?)) },
  Thread.new { vm1.step until vm1.halted? || (vm1.blocked? && (vm0.blocked? || vm0.halted?)) },
].each(&:join)
puts "p2: vm1 executed snd instruction #{vm1.snd_count} times"
