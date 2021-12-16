#!/usr/bin/env ruby

require "minitest/autorun"
require_relative "d16"

describe :parse_packets do
  it "parses example literal packet" do
    hex = "D2FE28"
    packets = parse_packets(hex_to_bin(hex))
    _(packets.count).must_equal(1)
    p = packets[0]
    _(p.version_bits).must_equal("110")
    _(p.type_bits).must_equal("100")
    _(p.type).must_equal(4)
    _(p.contents).must_equal("011111100101")
    _(p.value).must_equal(2021)
  end

  it "parses example operator packet with length type 0" do
    hex = "38006F45291200"
    packets = parse_packets(hex_to_bin(hex))
    _(packets.count).must_equal(1)
    p = packets[0]
    _(p.version_bits).must_equal("001")
    _(p.version).must_equal(1)
    _(p.type_bits).must_equal("110")
    _(p.type).must_equal(6)
    _(p.contents).must_be_instance_of(Array)
    _(p.contents.count).must_equal(2)
    subpacket1 = p.contents[0]
    _(subpacket1.type).must_equal(4)
    _(subpacket1.value).must_equal(10)
    subpacket2 = p.contents[1]
    _(subpacket2.type).must_equal(4)
    _(subpacket2.value).must_equal(20)
  end

  it "parses example operator packet with length type 1" do
    hex = "EE00D40C823060"
    packets = parse_packets(hex_to_bin(hex))
    _(packets.count).must_equal(1)
    p = packets[0]
    _(p.version_bits).must_equal("111")
    _(p.version).must_equal(7)
    _(p.type_bits).must_equal("011")
    _(p.type).must_equal(3)
    _(p.contents).must_be_instance_of(Array)
    _(p.contents.count).must_equal(3)
    subpacket1 = p.contents[0]
    _(subpacket1.type).must_equal(4)
    _(subpacket1.value).must_equal(1)
    subpacket2 = p.contents[1]
    _(subpacket2.type).must_equal(4)
    _(subpacket2.value).must_equal(2)
    subpacket3 = p.contents[2]
    _(subpacket3.type).must_equal(4)
    _(subpacket3.value).must_equal(3)
  end
end

describe Packet do
  it "gets the right value for a sum packet" do
    hex = "C200B40A82"
    p = parse_packet(hex_to_bin(hex))[0]
    _(p.value).must_equal(3)
  end

  it "gets the right value for a complex packet combining sum, product, equality" do
    hex = "9C0141080250320F1802104A08"
    p = parse_packet(hex_to_bin(hex))[0]
    _(p.value).must_equal(1)
  end
end
