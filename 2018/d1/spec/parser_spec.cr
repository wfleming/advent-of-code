require "./spec_helper"

describe Parser do
  describe "parsing" do
    it "parses lines into numbers" do
      parser = Parser.new
      output = parser.parse "+1\n+3\n-2\n"
      output.should eq([1, 3, -2])
    end
  end
end
