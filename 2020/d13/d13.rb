#!/usr/bin/env ruby

Input = Struct.new(:time, :buses) do
  def self.read(path)
    ls = File.readlines(path)
    new(
      Integer(ls[0]),
      ls[1].split(",").reject { |i| i == "x" }.map(&method(:Integer)),
    )
  end
end

def p1(input)
  bus_mod = input.buses.map { |b|
    m = input.time % b
    m = m == 0 ? m : b - m
    [b, m]
  }.sort_by { |x| x[1] }

  bus_mod[0]
end

input = Input.read(ARGV[0])
p1_bus, p1_bus_time = *p1(input)
puts "p1: first bus is #{p1_bus}, you'll wait #{p1_bus_time} mins. it leaves at #{input.time + p1_bus_time}"
puts "p1: bus * wait = #{p1_bus * p1_bus_time}"
