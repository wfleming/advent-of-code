require "./spec_helper"

describe Checksum do
  it "checksums short list" do
    list = [
      "abcdef",
      "bababc",
      "abbcde",
      "abcccd",
      "aabcdd",
      "abcdee",
      "ababab",
    ]

    Checksum.new(list).checksum.should eq(12)
  end
end
