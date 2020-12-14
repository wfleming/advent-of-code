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

  def run_program(instructions, version: 1)
    method = "run_instruction_v#{version}".to_sym
    instructions.each(&method(method))
  end

  def run_instruction_v1(instruction)
    if (m = MASK_PAT.match(instruction))
      @mask = m["mask"]
    elsif (m = MEM_PAT.match(instruction))
      memory[Integer(m["addr"])] = apply_mask_val(Integer(m["val"]))
    else
      raise "couldn't parse '#{instruction}'"
    end
  end

  def apply_mask_val(n)
    raise "should have a mask set before setting mem" if mask.nil?
    n_bin = "%0#{mask.length}b" % n

    mask.each_char.each_with_index do |char, idx|
      if %w[1 0].include?(char)
        n_bin[idx] = char
      end
    end

    Integer(n_bin, 2)
  end

  def run_instruction_v2(instruction)
    if (m = MASK_PAT.match(instruction))
      @mask = m["mask"]
    elsif (m = MEM_PAT.match(instruction))
      # puts "DEBUG: v2 mapped addr #{m["addr"]} with mask #{mask} to #{apply_mask_addr(m["addr"])}"
      apply_mask_addr(m["addr"]).each do |addr|
        memory[addr] = Integer(m["val"])
      end
    else
      raise "couldn't parse '#{instruction}'"
    end
  end

  def apply_mask_addr(n)
    raise "should have a mask set before setting mem" if mask.nil?
    n_bin = "%0#{mask.length}b" % n

    # flip the bits first
    mask.each_char.each_with_index do |char, idx|
      n_bin[idx] = char if %w[1 X].include?(char)
    end

    # expand the Xs
    expand_masked_addr(n_bin).map(&method(:Integer))
  end

  # I know this is a bad idea generally, see commit message
  def expand_masked_addr(str)
    return [""] if str.length == 0
    h, t = str[0], str[1..]

    # puts "DEBUG: expand_addr h=#{h} t=#{t}"
    if h == "X"
      expand_masked_addr(t).flat_map { |t2| ["1#{t2}", "0#{t2}"] }
    else
      expand_masked_addr(t).map { |t2| "#{h}#{t2}" }
    end
  end
end

# p1
p = Program.new
p.run_program(INSTRUCTIONS)
puts "p1: sum of memory is #{p.mem_sum}"

# p2
p2 = Program.new
p2.run_program(INSTRUCTIONS, version: 2)
puts "p2: sum of memory is #{p2.mem_sum}"
