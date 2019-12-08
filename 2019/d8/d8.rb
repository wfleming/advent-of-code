#!/usr/bin/env ruby
#
# to support different dims for test input
WIDTH = (ARGV[1] || 25).to_i
HEIGHT = (ARGV[2] || 6).to_i

# an array of layers
# for now each layer is just another array:
# don't need a grid for p1
def read_layers(digits)
  digits.each_slice(WIDTH * HEIGHT).to_a
end

def p1(layers)
  l = layers.min_by do |l|
    l.filter { |d| d == 0 }.count
  end

  ones = l.filter { |d| d == 1 }.count
  twos = l.filter { |d| d == 2 }.count

  ones * twos
end

layers = read_layers(
  File.read(ARGV[0]).strip.split("").map(&:to_i),
)

puts "p1: #{p1(layers)}"
