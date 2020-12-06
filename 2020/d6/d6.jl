#!/usr/bin/env julia

function load_groups()
  read(open(ARGS[1]), String) |>
    Base.Fix2(split, r"^$"m) |>
    Base.Fix1(map, strip) |>
    Base.Fix1(filter, x -> length(x) > 0)
end

# turn string into an array of arrays of characters
function parse_group(group)
  map(Base.Fix2(split, ""), split(group, "\n"))
end

function build_sum_qs(score_fn)
  function (groups)
    map(group -> foldl(score_fn, parse_group(group)), groups) |>
      Base.Fix1(map, length) |>
      sum
  end
end

p1_sum_qs = build_sum_qs(union)
p2_sum_qs = build_sum_qs(intersect)

groups = load_groups()
println("p1: sum is $(p1_sum_qs(groups))")
println("p2: sum is $(p2_sum_qs(groups))")
