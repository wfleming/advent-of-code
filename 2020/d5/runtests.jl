#!/usr/bin/env julia

using Test

include("boarding_pass.jl")

@testset "BoardingPass.split_range" begin
  @test BoardingPass.split_range(0:127) == (0:63, 64:127)
  @test BoardingPass.split_range(0:1) == (0:0, 1:1)
end

@testset "BoardingPass.parse" begin
  @test BoardingPass.parse("FBFBBFFRLR") == BoardingPass.Pass(44, 5)
  @test BoardingPass.parse("BFFFBBFRRR") == BoardingPass.Pass(70, 7)
  @test BoardingPass.parse("FFFBBBFRRR") == BoardingPass.Pass(14, 7)
  @test BoardingPass.parse("BBFFBBFRLL") == BoardingPass.Pass(102, 4)
end
