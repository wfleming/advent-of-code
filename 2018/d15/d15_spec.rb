require "minitest/autorun"

require "d15"

SIMPLE = <<~STR
#######
#.G.E.#
#E.G.E#
#.G.E.#
#######
STR

SIMPLE2 = <<~STR
#######
#E..G.#
#...#.#
#.G.#G#
#######
STR

SAMPLE_P1 = <<~STR
#######
#.G...#
#...EG#
#.#.#G#
#..G#E#
#.....#
#######
STR

describe Point do
  it "sorts" do
    (Point.new(1, 1) <=> Point.new(1, 1)).must_equal 0

    (Point.new(1, 1) <=> Point.new(2, 1)).must_equal -1

    (Point.new(1, 4) <=> Point.new(8, 1)).must_equal 1
  end
end

describe Map do
  it "parses" do
    m = Map.parse(SIMPLE)

    m.units.count.must_equal(7)

    u = m.units_move_order[0]
    u.pos.must_equal(Point.new(2, 1))
    u.type.must_equal("G")

    u = m.units_move_order[1]
    u.pos.must_equal(Point.new(4, 1))
    u.type.must_equal("E")
  end

  it "runs a single turn" do
    m = Map.parse(SIMPLE2)

    u = m.units_move_order[0]
    u.pos.must_equal(Point.new(1, 1))
    m.take_turn(u)

    u.pos.must_equal(Point.new(2, 1))
  end

  it "finds a correct path" do
    m = Map.parse(<<~STR)
    #######
    #######
    #.E..G#
    #.#####
    #G#####
    #######
    #######
    STR

    u = m.units_move_order[0]
    u.type.must_equal "E"

    p = m.target_square(u)
    p.must_equal([Point.new(2, 2), Point.new(3, 2), Point.new(4, 2)])
  end

  it "runs a full battle1" do
    m = Map.parse(SAMPLE_P1)

    until m.battle_over?
      m.tick_round
    end

    m.rounds.must_equal 47
    m.units.map(&:hp).sum.must_equal 590
  end

  it "runs a full battle2" do
    m = Map.parse(<<~STR)
    #######
    #G..#E#
    #E#E.E#
    #G.##.#
    #...#E#
    #...E.#
    #######
    STR

    until m.battle_over?
      m.tick_round
    end

    m.rounds.must_equal 37
    m.elves.count.must_be :>, 0
    m.goblins.count.must_equal 0
    m.units.map(&:hp).sum.must_equal 982
  end

  it "runs a full battle3" do
    m = Map.parse(<<~STR)
    #######
    #E..EG#
    #.#G.E#
    #E.##E#
    #G..#.#
    #..E#.#
    #######
    STR

    until m.battle_over?
      m.tick_round
    end

    m.rounds.must_equal 46
    m.elves.count.must_be :>, 0
    m.goblins.count.must_equal 0
    m.units.map(&:hp).sum.must_equal 859
  end

  it "runs a full battle4" do
    m = Map.parse(<<~STR)
    #######
    #E.G#.#
    #.#G..#
    #G.#.G#
    #G..#.#
    #...E.#
    #######
    STR

    until m.battle_over?
      m.tick_round
    end

    m.rounds.must_equal 35
    m.goblins.count.must_be :>, 0
    m.elves.count.must_equal 0
    m.units.map(&:hp).sum.must_equal 793
  end

  it "runs a full battle5" do
    m = Map.parse(<<~STR)
    #######
    #.E...#
    #.#..G#
    #.###.#
    #E#G#G#
    #...#G#
    #######
    STR

    until m.battle_over?
      m.tick_round
    end

    m.rounds.must_equal 54
    m.goblins.count.must_be :>, 0
    m.elves.count.must_equal 0
    m.units.map(&:hp).sum.must_equal 536
  end

  it "runs a full battle6" do
    m = Map.parse(<<~STR)
    #########
    #G......#
    #.E.#...#
    #..##..G#
    #...##..#
    #...#...#
    #.G...G.#
    #.....G.#
    #########
    STR

    until m.battle_over?
      m.tick_round
    end

    m.rounds.must_equal 20
    m.goblins.count.must_be :>, 0
    m.elves.count.must_equal 0
    m.units.map(&:hp).sum.must_equal 937
  end
end

describe Astar do
  it "finds path, uses reading order for tie break" do
    m = Map.parse(<<~STR)
    #######
    #.E...#
    #.....#
    #...G.#
    #######
    STR

    u = m.units.first
    u.type.must_equal "E"

    p = Astar.search(
      start: u.pos,
      goal: Point.new(4, 2),
      cost_estimate_fn: m.method(:distance),
      neighbors_fn: m.method(:open_neighbors),
      distance_fn: m.method(:distance),
    )

    p.must_equal [Point.new(2, 1), Point.new(3, 1), Point.new(4, 1), Point.new(4, 2)]
  end
end

describe BFS do
  it "finds a path" do
    m = Map.parse(SAMPLE_P1)
    u = m.units_move_order[0]
    t = m.possible_target_squares(u)[0]

    u.pos.must_equal Point.new(2, 1)
    t.must_equal Point.new(4, 1)

    p = BFS.search(start: u.pos, goal: t, neighbors_fn: m.method(:open_neighbors))
    p.must_equal [[Point.new(2, 1), Point.new(3, 1), Point.new(4, 1)]]
  end
end
