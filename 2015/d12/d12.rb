#!/usr/bin/env ruby

require "json"

def find_nums(json, &block)
  case json
  when Hash
    json.flat_map { |_k, v| find_nums(v) }.compact
  when Array
    json.flat_map(&method(:find_nums)).compact
  when Numeric
    json
  else
    nil
  end
end

def find_nums_p2(json)
  case json
  when Hash
    unless json.values.include?("red")
      json.flat_map { |_k, v| find_nums_p2(v) }.compact
    end
  when Array
    json.flat_map(&method(:find_nums_p2)).compact
  when Numeric
    json
  else
    nil
  end
end

if __FILE__ == $0
  input = JSON.parse(File.read(ARGV[0]))

  p1 = find_nums(input).sum
  puts "p1: #{p1}"

  p2 = find_nums_p2(input).sum
  puts "p2: #{p2}"
end
