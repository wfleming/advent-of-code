#!/usr/bin/env ruby

require "minitest/spec"
require "minitest/autorun"
require_relative "d11.rb"

describe Validator do
  it "validates 'hijklmmn'" do
    v = Validator.new("hijklmmn")
    _(v.banned_chars?).must_equal(true)
    _(v.straight?).must_equal(true)
    _(v.pairs?).must_equal(false)
    _(v.valid?).must_equal(false)
  end

  it "validates 'abbceffg'" do
    v = Validator.new("abbceffg")
    _(v.banned_chars?).must_equal(false)
    _(v.straight?).must_equal(false)
    _(v.pairs?).must_equal(true)
    _(v.valid?).must_equal(false)
  end

  it "validates 'abbcegjk'" do
    v = Validator.new("abbcegjk")
    _(v.banned_chars?).must_equal(false)
    _(v.straight?).must_equal(false)
    _(v.pairs?).must_equal(false)
    _(v.valid?).must_equal(false)
  end
end

describe "next_valid_password" do
  it "finds next password from 'abcdefgh'" do
    p = next_valid_password("abcdefgh")
    _(p).must_equal("abcdffaa")
  end

  it "finds next password from 'ghijklmn'" do
    p = next_valid_password("ghijklmn")
    _(p).must_equal("ghjaabcc")
  end
end
