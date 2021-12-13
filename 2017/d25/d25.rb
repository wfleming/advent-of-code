#!/usr/bin/env ruby

StateAction = Struct.new(:write, :dir, :next_state)

Config = Struct.new(:start_state, :checksum_count, :states) do
  def self.parse(lines)
    start_state = /Begin in state ([A-Z])/.match(lines.shift)[1]
    checksum_count = /Perform a diagnostic checksum after (\d+) steps/.match(lines.shift)[1].to_i

    states = {}
    cur_state = nil
    cur_action = nil
    lines.each do |line|
      if m = /In state ([A-Z])/.match(line)
        cur_state = { 0 => StateAction.new, 1 => StateAction.new }
        states[m[1]] = cur_state
      elsif m = /If the current value is (\d)/.match(line)
        cur_action = cur_state[m[1].to_i]
      elsif m = /Write the value (\d)/.match(line)
        cur_action.write = m[1].to_i
      elsif m = /Move one slot to the (\w+)/.match(line)
        cur_action.dir = m[1] == "right" ? 1 : -1
      elsif m = /Continue with state ([A-Z])/.match(line)
        cur_action.next_state = m[1]
      end
    end

    new(start_state, checksum_count, states)
  end
end


class TuringMachine
  def initialize(config)
    @tape = Hash.new(0)
    @pos = 0
    @config = config
    @state = config.start_state
  end

  def checksum
    @tape.count { |_k, v| v == 1 }
  end

  def step_n(n)
    n.times { step }
  end

  def step
    action = @config.states[@state][@tape[@pos]]
    @tape[@pos] = action.write
    @pos += action.dir
    @state = action.next_state
  end
end

config = Config.parse(File.readlines(ARGV[0]))
machine = TuringMachine.new(config)

machine.step_n(config.checksum_count)
puts "p1: checksum is #{machine.checksum}"
