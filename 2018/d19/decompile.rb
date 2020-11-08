#!/usr/bin/env ruby

class Program
  def initialize(ip, stmts)
    @ip = ip
    @stmts = stmts
  end
end

class Pass
  def initialize(@program)
  end
end

# rewrite instructions to be a bit more readable
class Pass1
  def apply
  end
end

class Decompiler
  def initialize(str)
    lines = str.lines.map(&:strip)
    ip = lines.shift.split[1]
    stmts = lines.map(&:split)
    @program = Program.new(ip, stmts)
  end

  def decompile
    [
      Pass1
    ].each do |pass_klass|
      pass.new(@program).apply
    end
  end
end

if $0 == __FILE__
  Decompiler.new(File.read(ARGV[0])).decompile
end
