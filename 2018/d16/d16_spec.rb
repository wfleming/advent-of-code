require "minitest/autorun"

require "d16"

describe Parser do
  it "parses changes & instrs" do
    input = <<~STR
      Before: [0, 2, 2, 2]
      11 3 3 3
      After:  [0, 2, 2, 0]

      Before: [3, 2, 1, 1]
      11 2 3 3
      After:  [3, 2, 1, 0]


      14 3 3 2
      14 3 3 0
    STR

    parser = Parser.new(input)
    result = parser.parse

    result[:changes].must_equal [
      Change.new([0, 2, 2, 2], [11, 3, 3, 3], [0, 2, 2, 0]),
      Change.new([3, 2, 1, 1], [11, 2, 3, 3], [3, 2, 1, 0]),
    ]
    result[:instructions].must_equal [
      [14, 3, 3, 2],
      [14, 3, 3, 0],
    ]
  end
end

describe OpCodeCandidates do
  it "detects candidates for sample" do
    c = Change.new([3, 2, 1, 1], [9, 2, 1, 2], [3, 2, 2, 1])
    candidates = OpCodeCandidates.new(c).candidates
    candidates.sort.must_equal [:mulr, :addi, :seti].sort
  end
end
