const Point = @NamedTuple{x::Int64, y::Int64}

struct Maze
  paths::Array{Point}
  entrance::Point
  keys::Dict{Char, Point}
  doors::Dict{Char, Point}
end

function parse_maze_from_file(path)
  parse_maze_from_string(read(open(path), String))
end

function parse_maze_from_string(str)
  paths = []
  entrance = Point((-1, -1))
  keys = Dict()
  doors = Dict()

  # x starts at left, y starts at top
  lines = split(str, "\n")
  for y in eachindex(lines)
    for x in eachindex(lines[y])
      c = lines[y][x]
      pt = Point((x, y))
      if c == '.'
        push!(paths, pt)
      elseif c == '@'
        entrance = pt
        push!(paths, pt)
      elseif islowercase(c)
        keys[c] = pt
        push!(paths, pt)
      elseif isuppercase(c)
        doors[c] = pt
        push!(paths, pt)
      end
    end
  end

  Maze(paths, entrance, keys, doors)
end

function pt_distance(a::Point, b::Point)
  abs(a.x - b.x) + abs(a.y - b.y)
end

function pt_neighbors(pt::Point)::Array{Point}
  [
    Point((pt.x - 1, pt.y)),
    Point((pt.x + 1, pt.y)),
    Point((pt.x, pt.y - 1)),
    Point((pt.x, pt.y + 1)),
  ]
end

function maze_at(maze::Maze, pt::Point):: Bool
  any(p -> (pt == p), maze.paths)
end
