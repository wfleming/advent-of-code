#!/usr/bin/env julia

include("boarding_pass.jl")

load_input() = eachline(open(ARGS[1]))

# p1

highest_id = map(BoardingPass.parse, load_input()) |>
  Base.Fix1(map, BoardingPass.seat_id) |>
  maximum
println("p1: highest id is $highest_id")

# p2

all_seats = Base.product(BoardingPass.ROW_RANGE, BoardingPass.COL_RANGE) |>
  Base.Fix1(map, pair -> BoardingPass.Pass(pair[1], pair[2]))
filled_seats = map(BoardingPass.parse, load_input())
filled_seat_ids = map(BoardingPass.seat_id, filled_seats)
missing_seats = filter(!in(filled_seats), all_seats)
our_seat = filter(
  s -> begin
    id = BoardingPass.seat_id(s)
    id + 1 in filled_seat_ids && id - 1 in filled_seat_ids
  end,
  missing_seats
)

if length(our_seat) != 1
  error("filtering by neigboring ids should have been unique")
end

our_seat = first(our_seat)
println("p2: our seat is at $our_seat, id is $(BoardingPass.seat_id(our_seat))")
