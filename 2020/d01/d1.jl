#!/usr/bin/env julia

function load_expenses()
  map(Base.Fix1(parse, Int64), eachline("input.txt"))
end

function find_pair(expenses, target)
  for (x,y) in Base.Iterators.product(expenses, expenses)
    if target == x + y
      return (x, y)
    end
  end

  nothing
end

function find_triple(expenses, target)
  for x in expenses
    p = find_pair(expenses, target - x)
    if p != nothing
      return (x, p[1], p[2])
    end
  end

  nothing
end

expenses = load_expenses()

# p1
ns = find_pair(expenses, 2020)
println("p1: found $ns, the product is $(prod(ns))")

# p2
ns = find_triple(expenses, 2020)
println("p2: found $ns, the product is $(prod(ns))")
