require "d16"
require "pry" #DEBUG

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
    while (op = @instructions[ip])
      tick
    end
  end

  def tick
    if (op = @instructions[ip])
      apply(op)
      true
    else
      false
    end
  end

  def apply(op)
    registers[ip_reg] = ip
    super
    @ip = registers[ip_reg] + 1
  end
end

class Compiler
  attr_reader :machine

  def initialize(machine)
    @machine = machine
  end

  # return str
  def compile
    # beginning boilerplate
    src = <<~HEADER
    #include <stdlib.h>
    #include <stdio.h>

    typedef struct {
      int ipreg, ip;
      int rs[6];
    } State;
    HEADER

    # write functions for processing
    machine.instructions.each_with_index do |op, idx|
      src << "\n#{op_fn_def(op, idx)}\n"
    end
    src << "\n#{run_fn_def}\n\n"

    # main function
    src << <<~MAIN
    int main() {
      // init State
      State *s = malloc(sizeof(State));
      s->ipreg = #{machine.ip_reg};
      s->ip = #{machine.ip};
    MAIN

    machine.registers.each_with_index do |r, idx|
      src << "  s->rs[#{idx}] = #{r};\n"
    end

    src << <<~SRC

      // run machine
      while(s->ip >= 0 && s->ip < #{machine.instructions.count}) {
        s->rs[s->ipreg] = s->ip;
        run_op(s);
        s->ip = s->rs[s->ipreg] + 1;
      }

      // print result
      printf("reg 0 is %d\\n", s->rs[0]);
      return 0;
    }
    SRC

    src
  end

  def run_fn_def
    src = <<~SRC
    void run_op(State *s) {
      switch (s->ip) {
    SRC

    machine.instructions.each_with_index do |_, idx|
      src << indent(<<~SRC, 2) + "\n"
        case #{idx}:
          op_#{idx}(s);
          break;
      SRC
    end

    src << "  }\n\n}"
    src
  end

  def op_fn_def(op, idx)
    src = <<~SRC
    void op_#{idx}(State *s) {
    SRC

    body =
      case op[0]
      when "addr"
        "s->rs[#{op[3]}] = s->rs[#{op[1]}] + s->rs[#{op[2]}];"
      when "addi"
        "s->rs[#{op[3]}] = s->rs[#{op[1]}] + #{op[2]};"
      when "mulr"
        "s->rs[#{op[3]}] = s->rs[#{op[1]}] * s->rs[#{op[2]}];"
      when "muli"
        "s->rs[#{op[3]}] = s->rs[#{op[1]}] * #{op[2]};"
      when "banr"
        "s->rs[#{op[3]}] = s->rs[#{op[1]}] & s->rs[#{op[2]}];"
      when "bani"
        "s->rs[#{op[3]}] = s->rs[#{op[1]}] & #{op[2]};"
      when "borr"
        "s->rs[#{op[3]}] = s->rs[#{op[1]}] | s->rs[#{op[2]}];"
      when "bori"
        "s->rs[#{op[3]}] = s->rs[#{op[1]}] | #{op[2]};"
      when "setr"
        "s->rs[#{op[3]}] = s->rs[#{op[1]}];"
      when "seti"
        "s->rs[#{op[3]}] = #{op[1]};"
      when "gtir"
        <<~SRC
          if (#{op[1]} > s->rs[#{op[2]}]) {
            s->rs[#{op[3]}] = 1;
          } else {
            s->rs[#{op[3]}] = 0;
          }
        SRC
      when "gtri"
        <<~SRC
          if (s->rs[#{op[1]}] > #{op[2]}) {
            s->rs[#{op[3]}] = 1;
          } else {
            s->rs[#{op[3]}] = 0;
          }
        SRC
      when "gtrr"
        <<~SRC
          if (s->rs[#{op[1]}] > s->rs[#{op[2]}]) {
            s->rs[#{op[3]}] = 1;
          } else {
            s->rs[#{op[3]}] = 0;
          }
        SRC
      when "eqir"
        <<~SRC
          if (#{op[1]} == s->rs[#{op[2]}]) {
            s->rs[#{op[3]}] = 1;
          } else {
            s->rs[#{op[3]}] = 0;
          }
        SRC
      when "eqri"
        <<~SRC
          if (s->rs[#{op[1]}] == #{op[2]}) {
            s->rs[#{op[3]}] = 1;
          } else {
            s->rs[#{op[3]}] = 0;
          }
        SRC
      when "eqrr"
        <<~SRC
          if (s->rs[#{op[1]}] == s->rs[#{op[2]}]) {
            s->rs[#{op[3]}] = 1;
          } else {
            s->rs[#{op[3]}] = 0;
          }
        SRC
      end
    src << indent(body)
    src << "\n}"
  end

  def indent(str, level = 1)
    str.lines.map do |l|
      ("  " * level) + l.rstrip
    end.join("\n")
  end
end

if $0 == __FILE__
  # m = Machine19.parse(File.read(ARGV[0]))
  # m.run
  # puts "p1: registers after halt are #{m.registers}"

  m = Machine19.parse(File.read(ARGV[0]))
  m.registers[0] = 1
  File.open("d19p2.c", "w") { |fh| fh.write(Compiler.new(m).compile) }
  puts "p2: c source written d19p2.c, compile and run that"
end
