#!/usr/bin/env ruby

def code_at(row, col)
  code = 20151125
  cur_row, cur_col = *[1, 1]

  while cur_row != row || cur_col != col
    cur_row, cur_col =
      if cur_row == 1
        [cur_col + 1, 1]
      else
        [cur_row - 1, cur_col + 1]
      end
    code = (code * 252533) % 33554393
  end

  code
end

# verify
# tbl = (1..6).map do |r|
#   (1..6).map do |c|
#     code_at(r, c)
#   end
# end

# tbl.each do |row|
#   puts row.join("  ")
# end
# return
# end verification

p1_code = code_at(2981, 3075)
puts "p1: code at (2981, 3075) = #{p1_code}"
