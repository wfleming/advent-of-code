include("maze.jl")

struct Explorer
  maze::Maze
  keys::Array{Char}
  position::Point
end

Explorer(m::Maze) = Explorer(m, [], m.entrance)

function step(explorer::Explorer, new_pos::Point)
  # don't take more than 1 step
  if pt_distance(explorer.position, new_pos) != 1
    error("new_pos $new_pos is not a neighbor of current position $explorer.position")
  end

  # refuse to walk into a door you can't open
  door = findfirst(v -> v == new_pos, explorer.maze.doors)
  if door != nothing && !(lowercase(door) in explorer.keys)
    error("step would take us to door $door but we don't have key for it")
  end

  new_keys = copy(explorer.keys)
  found_key = findfirst(v -> v == new_pos, explorer.maze.keys)
  if found_key != nothing
    push!(new_keys, found_key)
  end

  Explorer(
    explorer.maze,
    new_keys,
    new_pos
  )
end

function can_step(explorer::Explorer, pt:: Point):: Bool
  is_path = maze_at(explorer.maze, pt)

  if !is_path
    return false
  end

  door = findfirst(p -> p == pt, explorer.maze.doors)
  door == nothing || lowercase(door[1]) in explorer.keys
end

# use pt_neighors, filter by paths & doors/keys
function available_steps(explorer::Explorer)
  filter(
    pt -> can_step(explorer, pt),
    pt_neighbors(explorer.position)
  )
end

# this solves p1
function find_best_path_all_keys(maze::Maze)
  open_set = [Explorer(maze)]

  # the array is points passed through to get there, length() of that array is
  # number of steps takes
  g_score = Dict(open_set[1] => []) #::Dict{Explorer, Array{Point}}
  # f score is length(g_score) + estimated_cost
  f_score = Dict(open_set[1] => estimated_cost(open_set[1])) #::Dict{Explorer, Integer}

  while length(open_set) > 0
    # current = open_set node with lowest f score
    sort!(open_set, by = e -> f_score[e])
    current = popat!(open_set, 1)

    if goal_reached(current)
      return g_score[current]
    end

    path_so_far = g_score[current]
    for next_pos = available_steps(current)
      next_g_score = vcat(path_so_far, [next_pos])
      next_state = step(current, next_pos)

      if !(next_state in keys(g_score)) || length(g_score[next_state]) < length(next_g_score)
        println("DEBUG - open_set_len=$(length(open_set)) best_g_score=$(length(next_g_score))")
        g_score[next_state] = next_g_score
        f_score[next_state] = length(next_g_score) + estimated_cost(next_state)
        if !(next_state in open_set)
          push!(open_set, next_state)
        end
      end
    end
  end
end

# sum the distance from current position to all remaining keys
# this is used to estimate the f score.
function estimated_cost(explorer::Explorer)
  keys_left = filter(k -> !(k in explorer.keys), collect(keys(explorer.maze.keys)))
  keys_left_pos = map(k -> explorer.maze.keys[k], keys_left)
  # println("DEBUG: estimating cost, keys_left_pos = $keys_left_pos")
  map(p -> pt_distance(explorer.position, p), keys_left_pos) |> sum
end

function goal_reached(explorer::Explorer):: Bool
  all_keys = explorer.maze.keys |> keys |> collect
  all(k -> k in explorer.keys, all_keys)
end
