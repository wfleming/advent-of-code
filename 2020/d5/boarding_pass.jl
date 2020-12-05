module BoardingPass
  struct Pass
    row::Int64
    col::Int64
  end

  ROW_RANGE = 0:127
  COL_RANGE = 0:7

  function split_range(r):: Tuple{UnitRange, UnitRange}
    if !iseven(length(r))
      error("we should only be splitting ranges of even lengths")
    end

    mid_point = first(r) + div(length(r), 2)
    (first(r):(mid_point - 1), mid_point:last(r))
  end

  function parse(str::String)::Pass
    row_range = ROW_RANGE
    col_range = COL_RANGE

    for c in str
      if c == 'F' # keep lower half of row range
        row_range = split_range(row_range)[1]
      elseif c == 'B' # keep upper half of row range
        row_range = split_range(row_range)[2]
      elseif c == 'L' # keep lower half of col range
        col_range = split_range(col_range)[1]
      elseif c == 'R' # keep upper half of col range
        col_range = split_range(col_range)[2]
      else
        error("character $c is invalid in boarding pass $str")
      end
    end

    # both ranges should be length 1 now
    if length(row_range) != 1 || length(col_range) != 1
      error("boarding pass $str didn't narrow to one seat: row=$row_range col=$col_range")
    end

    Pass(first(row_range), first(col_range))
  end

  function seat_id(pass)
    (pass.row * 8) + pass.col
  end
end
