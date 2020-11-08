require "minitest/autorun"

require "d19"

SAMPLE = <<~STR
#ip 0
seti 5 0 1
seti 6 0 2
addi 0 1 0
addr 1 2 3
setr 1 0 0
seti 8 0 4
seti 9 0 5
STR

describe Machine19 do
  it "parses and runs" do
    m = Machine19.parse(SAMPLE)

    m.ip_reg.must_equal 0
    m.ip.must_equal 0
    m.registers.must_equal [0, 0, 0, 0, 0, 0]

    m.tick.must_equal true
    m.ip.must_equal 1
    m.registers.must_equal [0, 5, 0, 0, 0, 0]

    m.tick.must_equal true
    m.ip.must_equal 2
    m.registers.must_equal [1, 5, 6, 0, 0, 0]

    m.tick.must_equal true
    m.ip.must_equal 4
    m.registers.must_equal [3, 5, 6, 0, 0, 0]

    m.tick.must_equal true
    m.ip.must_equal 6
    m.registers.must_equal [5, 5, 6, 0, 0, 0]

    m.tick.must_equal true
    m.ip.must_equal 7
    m.registers.must_equal [6, 5, 6, 0, 0, 9]

    m.tick.must_equal false
  end

  it "runs p2" do
    m = Machine19.parse(SAMPLE)
    m.registers[0] = 1
    m.run
  end
end
