#!/usr/bin/env julia

function load_groups()
  read(open(ARGS[1]), String) |>
    Base.Fix2(split, r"^$"m) |>
    Base.Fix1(map, strip) |>
    Base.Fix1(filter, x -> length(x) > 0)
end

function group_questions_answered_union(group)
  qs = Set()

  for m in eachmatch(r"[a-z]", group)
    push!(qs, m.match)
  end

  qs
end

function p1_sum_qs(groups)
  map(group_questions_answered_union, groups) |>
    Base.Fix1(map, length) |>
    sum
end

function group_questions_answered_intersect(group)
  # an array of arrays of characters
  individuals = map(Base.Fix2(split, ""), split(group, "\n"))

  foldl(intersect, individuals)
end

function p2_sum_qs(groups)
  map(group_questions_answered_intersect, groups) |>
    Base.Fix1(map, length) |>
    sum
end

groups = load_groups()
println("p1: sum is $(p1_sum_qs(groups))")
println("p2: sum is $(p2_sum_qs(groups))")
