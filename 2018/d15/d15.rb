Point = Struct.new(:x, :y) do
  def to_s
    "(#{x}, #{y})"
  end

  def <=>(other)
    if (c = (y <=> other.y)) == 0
      x <=> other.x
    else
      c
    end
  end
end

class Unit
  attr_reader :type, :dmg
  attr_accessor :pos, :hp

  def initialize(type, pos, dmg = 3)
    @type = type
    @pos = pos
    @hp = 200
    @dmg = dmg
  end

  def to_s
    "<Unit type=#{type} pos=#{pos} hp=#{hp}>"
  end

  def takes_hit(other)
    self.hp = hp - other.dmg
  end

  def alive?
    hp > 0
  end
end

ElfDied = Class.new(StandardError)

class Map
  def self.parse(str, elf_damage: 3)
    units = []
    walls = []

    str.lines.each_with_index do |line, y|
      line.each_char.each_with_index do |c, x|
        if c == "#"
          walls << Point.new(x, y)
        elsif %w[E G].include?(c)
          d = (c == "E" ? elf_damage : 3)
          units << Unit.new(c, Point.new(x, y), d)
        end
      end
    end

    new(walls, units)
  end

  attr_reader :walls, :units, :rounds

  attr_accessor :elf_dmg, :raise_on_elf_death

  def initialize(walls, units)
    @walls = walls
    @units = units
    @rounds = 0
    @elf_dmg = 3
  end

  def [](pos)
    walls.detect { |w| w == pos } ||
      units.detect { |u| u.pos == pos && u.alive? }
  end

  def elves
    units.select { |u| u.type == "E" }
  end

  def goblins
    units.select { |u| u.type == "G" }
  end

  def units_move_order
    units.sort_by { |u| [u.pos.y, u.pos.x] }
  end

  def enemies_of(unit)
    units.select { |u| (u.type != unit.type) && u.alive? }
  end

  def neighbors(pos)
    # in reading order
    [
      Point.new(pos.x, pos.y - 1),
      Point.new(pos.x - 1, pos.y),
      Point.new(pos.x + 1, pos.y),
      Point.new(pos.x, pos.y + 1),
    ]
  end

  def open_neighbors(pos)
    neighbors(pos).reject do |p|
      walls.include?(p) || units.any? { |u| u.pos == p && u.alive? }
    end
  end

  def target_enemy(unit)
    # prioritize enemies by hp, then reading order pos
    neighbors(unit.pos).map do |p|
      units.find { |u| u.pos == p && u.type != unit.type && u.alive? }
    end.compact.sort_by { |u| [u.hp, u.pos] }.first
  end

  def enemy_pos(unit)
    enemies_of(unit).map(&:pos)
  end

  # all squares a unit might want to target (open and next to an enemy)
  def possible_target_squares(unit)
    enemies_of(unit).flat_map do |e|
      open_neighbors(e.pos)
    end.uniq.sort_by { |p| distance(p, unit.pos) }
  end

  def distance(p1, p2)
    (p1.x - p2.x).abs + (p1.y - p2.y).abs
  end

  # return array of paths, where target is the goal point
  # and path is a list of points to travel to get there
  def target_paths_astar(unit)
    possible_target_squares(unit).map do |target|
      Astar.search(
        start: unit.pos,
        goal: target,
        cost_estimate_fn: method(:distance),
        neighbors_fn: method(:open_neighbors),
        distance_fn: method(:distance),
      )
    end.compact
  end

  # bread first search for a path that hits any target
  def target_paths_bfs(unit)
    goals = possible_target_squares(unit)
    BFS.search(
      start: unit.pos,
      goal: ->(path) { goals.include?(path[-1]) },
      neighbors_fn: method(:open_neighbors),
    )
  end

  def sorted_paths(paths)
    paths.sort_by { |path| [path.count, path[-1], path] }
  end

  def target_path(unit)
    Fuckit.search(
      start: unit.pos,
      goals: possible_target_squares(unit),
      neighbors_fn: method(:open_neighbors),
    )
  end

  # determine square unit wants to get to & how to get there
  def target_square(unit)
    # all possible targets, with costs for getting there, preferred in order of:
    # shortest path, reading-order goal, reading-order path steps
    # sorted_paths(target_paths_astar(unit)).first
    # sorted_paths(target_paths_bfs(unit)).first
    target_path(unit)
  end

  def take_turn(unit)
    enemy = target_enemy(unit)
    if enemy.nil? && (path = target_square(unit))
      if path[0] != unit.pos
        raise "path seems weird! should start at current pos. unit=#{unit}, path=#{path}"
      end
      # puts "R: #{rounds} #{unit.type} #{unit.pos} moves to #{path[1]}"
      unit.pos = path[1]

      # can attack after moving
      enemy = target_enemy(unit)
    end

    unless enemy.nil?
      # puts "R: #{rounds} #{unit.type} #{unit.pos} hits #{enemy.type} #{enemy.pos}"
      enemy.takes_hit(unit)
    end
  end

  def battle_over?
    units.select(&:alive?).map(&:type).uniq.count < 2
  end

  def tick_round
    units_moved = 0
    round_completed = true
    units_move_order.each do |unit|
      next unless unit.alive?
      if battle_over?
        round_completed = false
        break
      end
      take_turn(unit)
      units_moved += 1
    end

    if raise_on_elf_death && units.any? { |u| u.type == "E" && !u.alive? }
      raise ElfDied
    end

    # clean up dead units
    units.reject! { |u| !u.alive? }

    if !battle_over? && units_moved == 0
      raise "deadlock: nobody moved, but battle isn't over"
    end

    @rounds += 1 if round_completed
  end

  # debug: represent batle as string
  def viz
    w = walls.map(&:x).max
    h = walls.map(&:y).max

    # construct map
    lines = []
    (0..h).each do |y|
      line = ""
      (0..w).each do |x|
        occupant = self[Point.new(x, y)]
        if occupant.is_a?(Point)
          line << "#"
        elsif occupant.is_a?(Unit)
          line << occupant.type
        else
          line << "."
        end
      end

      # add HP on the right of the line
      line << "  "
      units_move_order.select { |u| u.pos.y == y }.each do |unit|
        line << "  #{unit.type}(#{unit.hp})"
      end

      lines << line
    end

    lines.join("\n")
  end
end

module BFS
  # returns array of shortest paths, or nil
  # goal can be a Proc or an object for comparison
  # say, a path length becomes longer than some other better destination path
  def self.search(start:, goal:, neighbors_fn:)
    # puts "BFS start=#{start} goal=#{goal}"
    paths = [[start]]

    while paths.any? && paths.none? { |p| self.winner?(p, goal) }
      # puts "  currently #{paths.count} paths, of len = #{paths[0].count}"
      # puts "  current paths = #{paths.map { |p| p.map(&:to_s) }}"
      paths = paths.flat_map do |path|
        neighbors_fn.call(path[-1]).reject do |point|
          path.include?(point) # never walk over same point twice
        end.map do |next_point|
          path + [next_point]
        end
      end.compact
    end

    # puts "  BFS start=#{start} goal=#{goal} is terminating"
    # puts " all_paths_left = \n#{paths.map{ |p| "    " + p.to_s }.join("\n")}"
    self.winners(paths, goal)
  end

  def self.winner?(path, goal)
    if goal.respond_to?(:call)
      goal.call(path)
    else
      path[-1] == goal
    end
  end

  def self.winners(paths, goal)
    paths.select { |p| self.winner?(p, goal) }
  end
end

# https://old.reddit.com/r/adventofcode/comments/a6chwa/2018_day_15_solutions/ebtwcqr/
# Looks kinda like djikstra? / AStar
# I think the key insight might be the hash tracking steps tracks dist & pos,
# and compares both, so it can properly prefer "reading order" steps
module Fuckit
  def self.search(start:, goals:, neighbors_fn:)
    open_set = [[start, 0]]
    closed_set = []
    distances = Hash.new # pos => [min_dist, from_pos]

    until open_set.empty?
      current, dist = *open_set.shift
      neighbors_fn.call(current).each do |nb|
        if !distances.key?(nb) || (distances[nb] <=> [dist + 1, current]) > 0
          distances[nb] = [dist + 1, current]
        end
        if !closed_set.include?(nb) && open_set.none? { |p| p[0] == nb }
          open_set << [nb, dist + 1]
        end
      end
      closed_set << current
    end

    # pick the target by least steps / reading order
    targets = distances.map do |dest, from|
      if goals.include?(dest)
        [from[0], dest]
      end
    end.compact.sort

    return nil if targets.none?
    target = targets[0][1]

    # build up the path to send back
    path = [target]
    while distances.key?(path[-1])
      path << distances[path[-1]][1]
    end
    path.reverse
  end
end

# https://en.wikipedia.org/wiki/A*_search_algorithm
module Astar
  def self.search(start:, goal:, cost_estimate_fn:, neighbors_fn:, distance_fn:)
    closed_set = []
    open_set = [start]

    came_from = Hash.new
    g_score = Hash.new(Float::INFINITY)
    g_score[start] = 0
    f_score = Hash.new(Float::INFINITY)
    f_score[start] = cost_estimate_fn.call(start, goal)

    until open_set.empty?
      current = open_set.sort_by { |n| f_score[n] }.first
      if current == goal
        return self.reconstruct_path(came_from, current)
      end

      open_set.delete(current)
      closed_set << current

      neighbors_fn.call(current).each do |neighbor|
        next if closed_set.include?(neighbor)

        tentative_gscore = g_score[current] + distance_fn.call(current, neighbor)

        if !open_set.include?(neighbor)
          open_set << neighbor
        elsif tentative_gscore >= g_score[neighbor]
          next
        end

        # this new path is better, record it
        came_from[neighbor] = current
        g_score[neighbor] = tentative_gscore
        f_score[neighbor] = g_score[neighbor] + cost_estimate_fn.call(neighbor, goal)
      end
    end
  end

  def self.reconstruct_path(came_from, current)
    path = [current]
    while came_from.key?(current)
      current = came_from[current]
      path << current
    end
    path.reverse
  end
end

def p1
  map = Map.parse(File.read(ARGV[0]))

  progress_printed = false
  until map.battle_over?
    map.tick_round

    print "\033[1A\033[" if progress_printed
    elves = map.elves
    goblins = map.goblins
    puts "_ battle: R=#{map.rounds} E=#{elves.count}, EHP=#{elves.map(&:hp).sum}, G=#{goblins.count} GHP=#{goblins.map(&:hp).sum}"
    progress_printed = true
  end

  p1_hp = map.units.map(&:hp).sum
  puts "p1: finished #{map.rounds} rounds, #{p1_hp} left, result = #{map.rounds * p1_hp}              "
end

def p2
  d = 10 # TODO back to 4
  progress_printed = false

  while true
    begin
      map = Map.parse(File.read(ARGV[0]), elf_damage: d)
      map.raise_on_elf_death = true

      until map.battle_over?
        map.tick_round

        print "\033[1A\033[" if progress_printed
        elves = map.elves
        goblins = map.goblins
        puts "_ battle: ED=#{d} R=#{map.rounds} E=#{elves.count}, EHP=#{elves.map(&:hp).sum}, G=#{goblins.count} GHP=#{goblins.map(&:hp).sum}"
        progress_printed = true
      end

      break # battle finished successfully, break out of while loop
    rescue ElfDied
      d += 1
    end
  end

  puts "p2: elves need #{d} damage to win without losses"
  p2_hp = map.units.map(&:hp).sum
  puts "p2: finished #{map.rounds} rounds, #{p2_hp} left, result = #{map.rounds * p2_hp}              "
end

if $0 == __FILE__
  p1
  p2
end
