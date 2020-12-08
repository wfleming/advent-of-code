require "delegate"

module Machine
  class Program < ::SimpleDelegator
    def clone
      self.class.new(map(&:clone))
    end

    def freeze
      map(&:clone).freeze
    end

    def self.parse(str)
      self.new(
        str.lines.map do |l|
          l.split(" ").each_with_index.map do |token, idx|
            if idx == 0
              token.to_sym
            else
              Integer(token)
            end
          end
        end
      )
    end
  end

  class VM
    attr_reader :reg_acc, :ip, :program

    def initialize(program)
      @ip = 0
      @reg_acc = 0
      @program = program.freeze
    end

    def step
      instr = program[ip]

      case instr[0]
      when :nop
        @ip += 1
      when :acc
        @reg_acc += instr[1]
        @ip += 1
      when :jmp
        @ip += instr[1]
      else
        raise ArgumentError, "encountered invalid instruction #{instr}"
      end
    end

    def exited?
      ip == program.count
    end
  end
end
