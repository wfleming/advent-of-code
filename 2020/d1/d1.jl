#!/usr/bin/env julia

function load_expenses()
  map(s -> parse(Int64, s), eachline("input.txt"))
end

function p1_find_matching_expenses()
  expenses = load_expenses()

  for (x,y) in Base.Iterators.product(expenses, expenses)
    if 2020 == x + y
      return (x, y)
    end
  end

  error("should have found pair summing to 2020")
end

function p2_find_matching_expenses()
  expenses = load_expenses()

  for (x,y,z) in Base.Iterators.product(expenses, expenses, expenses)
    if 2020 == x + y + z
      return (x, y, z)
    end
  end

  error("should have found triple summing to 2020")
end

# p1
ns = p1_find_matching_expenses()
println("p1: found $ns, the product is $(ns[1] * ns[2])")

# p2
ns = p2_find_matching_expenses()
println("p2: found $ns, the product is $(ns[1] * ns[2] * ns[3])")
