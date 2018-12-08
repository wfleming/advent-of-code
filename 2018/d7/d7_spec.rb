require "minitest/autorun"

require "d7"

TEST_INPUT = <<~INPUT
Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.
INPUT

describe "#parse" do
  it "parses" do
    deps = parse(TEST_INPUT)

    deps.must_equal({
      "A" => %w[C],
      "B" => %w[A],
      "D" => %w[A],
      "E" => %w[B D F],
      "F" => %w[C],
    })
  end
end

describe "Machine" do
  it "runs to completion" do
    deps = parse(TEST_INPUT)
    m = Machine.new(deps)
    m.run!

    m.state.done.join("").must_equal "CABDFE"
  end
end
