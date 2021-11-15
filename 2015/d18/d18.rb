#!/usr/bin/env ruby

def next_generation(grid, stuck=[])
  grid.each_with_index.map do |line, y|
    line.each_with_index.map do |light, x|
      neighbor_coords = [
        [y + 1, x - 1],
        [y + 1, x],
        [y + 1, x + 1],
        [y, x - 1],
        [y, x + 1],
        [y - 1, x - 1],
        [y - 1, x],
        [y - 1, x + 1],
      ].reject { |p| p[0] < 0 || p[1] < 0 || p[0] >= grid.count || p[1] >= line.count }
      neighbors = neighbor_coords.map { |p| grid[p[0]][p[1]] }

      if stuck.include?([y,x])
        true
      elsif light
        [2,3].include?(neighbors.select(&:itself).count)
      else
        neighbors.select(&:itself).count == 3
      end
    end
  end
end

# grid is addressed [y][x] (line, col)
initial_grid = File.readlines(ARGV[0]).map do |l|
  l.chomp.chars.map { |c| c == "#" ? true : false }
end

grid_p1 = 100.times.inject(initial_grid) { |memo, _x| next_generation(memo) }
lit = grid_p1.map { |line| line.count(&:itself) }.sum
puts "p1: #{lit} lights are lit"

stuck_pos = [[0, 0], [0, initial_grid.count - 1], [initial_grid.count - 1, 0], [initial_grid.count - 1, initial_grid.count - 1]]
initial_grid_p2 = initial_grid.map(&:clone)
stuck_pos.each { |y, x| initial_grid_p2[y][x] = true }
# puts "DEBUG INITIAL"
# puts initial_grid_p2.map { |l| l.map { |c| c ? "#" : "." }.join("") }.join("\n")
# puts "END DEBUG"
grid_p2 = 100.times.inject(initial_grid_p2) { |memo, _x| next_generation(memo, stuck_pos) }
# puts "DEBUG STUCK = #{stuck_pos}"
# puts "DEBUG AFTER ITERATION"
# puts grid_p2.map { |l| l.map { |c| c ? "#" : "." }.join("") }.join("\n")
# puts "END DEBUG"
lit = grid_p2.map { |line| line.count(&:itself) }.sum
puts "p2: #{lit} lights are lit"
