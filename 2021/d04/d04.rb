#!/usr/bin/env ruby

class Board
  def self.parse(drawn_numbers, board_lines)
    board_nums = board_lines.map do |l|
      l.split(" ").map(&method(:Integer))
    end
    self.new(drawn_numbers, board_nums)
  end

  def initialize(drawn_numbers, board_numbers)
    @drawn_numbers = drawn_numbers
    # array of rows, so addressing is [r][c]
    @board_numbers = board_numbers
  end

  def to_s
    @board_numbers.map.with_index do |row, row_idx|
      row.map.with_index do |n, col_idx|
        if @marked_cells.include?([row_idx, col_idx])
          "\e[32m#{n.to_s.rjust(2)}\e[0m"
        else
          n.to_s.rjust(2)
        end
      end.join(" ")
    end.join("\n")
  end

  # return [row, col] or nil
  def find_num(n)
    @board_numbers.each_with_index do |row, row_idx|
      row.each_with_index do |num, col_idx|
        return [row_idx, col_idx] if n == num
      end
    end
    nil
  end

  def play_to_win_or_end!
    # array of [row, col] addresses marked as drawn
    @marked_cells = []
    @drawn_numbers.each do |n|
      cell = find_num(n)
      @marked_cells << cell if cell
      break if won?
    end
  end

  def won?
    # we've won if any row or col is full
    # hash of "{type}_{idx}" => # filled
    buckets = Hash.new(0)

    @marked_cells.each do |pos|
      buckets["row_#{pos[0]}"] += 1
      buckets["col_#{pos[1]}"] += 1

      return true if [
        buckets["row_#{pos[0]}"],
        buckets["col_#{pos[1]}"],
      ].include?(5)
    end

    false
  end

  def turns_to_win
    if won?
      winning_num = @board_numbers[@marked_cells[-1][0]][@marked_cells[-1][1]]
      @drawn_numbers.find_index(winning_num) + 1
    end
  end

  def score
    marked_numbers = @marked_cells.map { |pos| @board_numbers[pos[0]][pos[1]] }
    unmarked_nums = @board_numbers.flatten - marked_numbers

    unmarked_nums.sum * marked_numbers[-1]
  end
end

def all_boards
  lines = File.readlines(ARGV[0])

  draw_nums = lines[0].split(",").map(&method(:Integer))

  lines[2..-1].each_slice(6).map do |board_lines|
    Board.parse(draw_nums, board_lines)
  end
end

boards = all_boards.map { |b| b.tap(&:play_to_win_or_end!) }
boards = boards.select(&:won?).sort_by(&:turns_to_win)

puts "p1: first board to win is\n#{boards[0].to_s}"
puts "p1: final score of this board is #{boards[0].score}"

puts "\np2: last board to win is\n#{boards[-1].to_s}"
puts "p2: final score of this board is #{boards[-1].score}"
