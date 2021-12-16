#!/usr/bin/env ruby

Packet = Struct.new(:version_bits, :type_bits, :contents) do
  def version
    @version ||= version_bits.to_i(2)
  end

  def type
    @type ||= type_bits.to_i(2)
  end

  def value
    case type
    when 0 # sum
      contents.sum(&:value)
    when 1 # product
      contents.map(&:value).reduce(&:*)
    when 2 # minimum
      contents.map(&:value).min
    when 3 # maximum
      contents.map(&:value).max
    when 4 # literal
      contents.to_i(2)
    when 5 # greater than
      contents[0].value > contents[1].value ? 1 : 0
    when 6 # less than
      contents[0].value < contents[1].value ? 1 : 0
    when 7 # equal to
      contents[0].value == contents[1].value ? 1 : 0
    end
  end

  def summed_version
    if type == 4
      version
    else
      version + contents.sum(&:summed_version)
    end
  end

  def to_s
    case type
    when 0 # sum
      "(#{contents.map(&:to_s).join(" + ")})"
    when 1 # product
      "(#{contents.map(&:to_s).join(" * ")})"
    when 2 # minimum
      "[#{contents.map(&:to_s).join(", ")}].min"
    when 3 # maximum
      "[#{contents.map(&:to_s).join(", ")}].max"
    when 4 # literal
      contents.to_i(2).to_s
    when 5 # greater than
      "(#{contents.map(&:to_s).join(" > ")} ? 1 : 0)"
    when 6 # less than
      "(#{contents.map(&:to_s).join(" < ")} ? 1 : 0)"
    when 7 # equal to
      "(#{contents.map(&:to_s).join(" == ")} ? 1 : 0)"
    end
  end
end

# return [bits, remaining]
def parse_literal(str)
  data_bits = ""
  i = 0
  until str[i] == "0"
    data_bits << str[i+1, 4]
    i += 5
  end
  data_bits << str[i+1, 4]
  i += 5

  [data_bits, str[i..]]
end

# return [packet, remaining]
def parse_packet(str)
  version_bits = str[0, 3]
  type_bits = str[3, 3]
  type = type_bits.to_i(2)

  if type == 4 # literal
    data_bits, remaining_str = parse_literal(str[6..])
    [Packet.new(version_bits, type_bits, data_bits), remaining_str]
  else # sub-packets
    length_type = str[6]
    if length_type == "0" # next 15 bits contain number of bits to consume for packets
      sub_packets_bit_len = str[7,15].to_i(2)
      contents = parse_packets(str[7 + 15, sub_packets_bit_len])
      remaining_str = str[(7 + 15 + sub_packets_bit_len)..]
      [Packet.new(version_bits, type_bits, contents), remaining_str]
    else # next 11 bits represent number of sub-packets
      sub_packets_count = str[7,11].to_i(2)
      remaining_str = str[(7 + 11)..]
      contents = sub_packets_count.times.reduce([]) do |memo, _|
        p, remaining_str = parse_packet(remaining_str)
        memo + [p]
      end
      [Packet.new(version_bits, type_bits, contents), remaining_str]
    end
  end
end

def parse_packets(str)
  packets = []

  until str.empty? || (str.length < 11 && /^0+$/.match?(str))
    p, str = parse_packet(str)
    packets << p
  end

  packets
end

def hex_to_bin(str)
  str.each_char.map { |c| c.to_i(16).to_s(2).rjust(4, "0") }.join
end

if __FILE__ == $0
  hex_str = File.read(ARGV[0]).chomp
  bin_str = hex_to_bin(hex_str)

  packets = parse_packets(bin_str)
  if packets.count != 1
    puts "Error: should have parsed a single root packet"
    exit 1
  end
  root_packet = packets[0]
  puts "p1: sum of all versions = #{root_packet.summed_version}"
  puts "p2: root packet value = #{root_packet.value}"

  # just for fun
  # puts root_packet.to_s
end
