#!/usr/bin/env ruby

lines = File.read(ARGV[0]).lines.map(&:chomp)
literal_size = lines.map(&:length).sum
mem_size = lines.map(&method(:eval)).map(&:length).sum

puts "p1: literal_size=#{literal_size}, mem_size=#{mem_size}"
puts "p1: diff=#{literal_size - mem_size}"

expanded_size = lines.map(&:inspect).map(&:length).sum
puts "p2: expanded_size=#{expanded_size} literal_size=#{literal_size}"
puts "p2: diff=#{expanded_size - literal_size}"
