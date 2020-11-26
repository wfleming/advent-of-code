#!/usr/bin/env julia

using Test

include("maze.jl")
include("explore.jl")

@testset "maze parsing - basic" begin
  m = parse_maze_from_string(
"""
#########
#b.A.@.a#
#########
"""
  )

  @test m.entrance == Point((6, 2))
  @test m.doors == Dict('A' => Point((4, 2)))
  @test m.keys == Dict(
    'a' => Point((8, 2)),
    'b' => Point((2, 2))
  )
  @test m.paths == [
    Point((2, 2)),
    Point((3, 2)),
    Point((4, 2)),
    Point((5, 2)),
    Point((6, 2)),
    Point((7, 2)),
    Point((8, 2)),
  ]
end

@testset "exploration - basic" begin
  m = parse_maze_from_string(
"""
#########
#b.A.@.a#
#########
"""
  )

  @test length(find_best_path_all_keys(m)) == 8
end

@testset "exploration - complex" begin
  m = parse_maze_from_string(
"""
#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################
"""
  )

  @test length(find_best_path_all_keys(m)) == 136
end
