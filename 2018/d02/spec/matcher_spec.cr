require "./spec_helper"

describe Matcher do
  it "finds the matching ids" do
    result = Matcher.new(test_list).matched_ids

    result.should eq({"fghij", "fguij"})
  end

  it "identifies shared characters" do
    result = Matcher.new(test_list).shared_chars

    result.should eq("fgij")
  end
end

def test_list
  [
    "abcde",
    "fghij",
    "klmno",
    "pqrst",
    "fguij",
    "axcye",
    "wvxyz",
  ]
end
