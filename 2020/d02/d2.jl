#!/usr/bin/env julia

function load_passwords()
  map(identity, eachline(ARGS[1]))
end

function is_valid_p1(line)
  pat = r"(\d+)-(\d+) (\w): (\w+)"
  m = match(pat, line)
  min = parse(Int64, m[1])
  max = parse(Int64, m[2])
  needle = m[3][1]
  haystack = m[4]

  cnt = filter(c -> c == needle, haystack) |>
    length

  cnt >= min && cnt <= max
end

function count_valid_p1()
  filter(is_valid_p1, load_passwords()) |>
    length
end

function is_valid_p2(line)
  pat = r"(\d+)-(\d+) (\w): (\w+)"
  m = match(pat, line)
  pos1 = parse(Int64, m[1])
  pos2 = parse(Int64, m[2])
  needle = m[3][1]
  haystack = m[4]

  (haystack[pos1] == needle ||
    haystack[pos2] == needle) &&
    haystack[pos1] != haystack[pos2]
end

function count_valid_p2()
  filter(is_valid_p2, load_passwords()) |>
    length
end

println("p1: there are $(count_valid_p1()) valid passwords")
println("p2: there are $(count_valid_p2()) valid passwords")
