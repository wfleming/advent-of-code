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

# turn each layer into nested array, e.g. layer[col][row]
def layers_to_grids(layers)
  layers.map do |layer|
    layer.each_slice(WIDTH).to_a
  end
end

def merge_layers(layers)
  # the last non-transparent pixel wins, keep list reversed
  layers = layers_to_grids(layers)

  HEIGHT.times.map do |col|
    WIDTH.times.map do |row|
      l = layers.find { |l| l[col][row] != 2 }
      l[col][row]
    end
  end
end

# looks like this image is white-on-black (white text)
# for readability, print white/1 as *, others as space
def image_to_s(image)
  image.map do |row|
    row.map do |pixel|
      pixel == 1 ? "*" : " "
    end.join("")
  end.join("\n")
end

def p2(layers)
  image_to_s(merge_layers(layers))
end

layers = read_layers(
  File.read(ARGV[0]).strip.split("").map(&:to_i),
)

puts "p1: #{p1(layers)}"
puts "p2:\n#{p2(layers)}"
