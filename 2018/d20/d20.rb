require "set"
require "pqueue"

Point = Struct.new(:x, :y) do
  def to_s
    "(#{x}, #{y})"
  end
end

class Graph
  include Enumerable

  attr_reader :vertices

  def initialize(root)
    # vertices is a hash of pt => connected_pts
    @vertices = { root => Set.new }
  end

  def add_edge(id_a, id_b)
    vertices[id_a] ||= Set.new
    vertices[id_b] ||= Set.new

    vertices[id_a] << id_b
    vertices[id_b] << id_a
  end
end

class Parser
  attr_reader :input

  def initialize(str)
    @input = str.each_char.to_a
  end

  def build_graph
    root = Point.new(0, 0)
    g = Graph.new(root)

    input.shift if input[0] == "^"
    walk(g, input, root)

    g
  end

  def walk(graph, pattern, root_id, indent="")
    current_id = root_id
    while (c = pattern.shift)
      if $verbose
        print "\033[1A\033[" if @progress_printed
        @progress_printed = true
        puts "_ graph has #{graph.vertices.count} nodes"
      end

      case c
      when "N"
        current_id = Point.new(current_id.x, current_id.y - 1).tap do |pt2|
          graph.add_edge(current_id, pt2)
        end
      when "S"
        current_id = Point.new(current_id.x, current_id.y + 1).tap do |pt2|
          graph.add_edge(current_id, pt2)
        end
      when "W"
        current_id = Point.new(current_id.x - 1, current_id.y).tap do |pt2|
          graph.add_edge(current_id, pt2)
        end
      when "E"
        current_id = Point.new(current_id.x + 1, current_id.y).tap do |pt2|
          graph.add_edge(current_id, pt2)
        end
      when "(" # push onto stack
        walk(graph, pattern, current_id, indent + "  ")
      when "|" # alt route
        current_id = root_id
      when ")" # pop stack
        return
      when "$" # end of the regex
        break
      end
    end
  end
end

def viz(graph)
  x_range = graph.vertices.keys.minmax_by(&:x).map(&:x)
  y_range = graph.vertices.keys.minmax_by(&:y).map(&:y)

  width = (1 + x_range[1] - x_range[0]) * 2 + 1

  lines = ["#" * width]

  (y_range[0]..y_range[1]).each do |y|
    # line1 contains this y, line2 contains walls & doors to y + 1
    line1 = "#"
    line2 = "#"
    (x_range[0]..x_range[1]).each do |x|
      p = Point.new(x, y)
      edges = graph.vertices[p]

      if edges&.any?
        line1 << (p == Point.new(0,0) ? "X" : ".")
        right = Point.new(x + 1, y)
        if edges.include?(right)
          line1 << "|"
        else
          line1 << "#"
        end
        down = Point.new(x, y + 1)
        if edges.include?(down)
          line2 << "-#"
        else
          line2 << "##"
        end
      else
        line1 << "##"
        line2 << "##"
      end
    end
    lines << line1
    lines << line2
  end

  lines.join("\n")
end

# return a hash of node_id -> shortest distance from [0,0]
Pair = Struct.new(:id, :dist) do
  def <=>(other)
    dist <=> other.dist
  end
end

# basically djikstra
def best_distances(graph, start)
  progress_printed = false

  queue = PQueue.new # [pt, dist]
  queue << Pair.new(start, 0)
  best_from = Hash.new # node_id -> dist

  while queue.size > 0
    pair = queue.shift
    pos, d = pair.id, pair.dist

    next if best_from.key?(pos) && best_from[pos] <= d

    if $verbose
      print "\033[1A\033[" if progress_printed
      progress_printed = true
      puts "_ searching seen #{best_from.count}, #{queue.size} in queue"
    end

    best_from[pos] = d

    graph.vertices[pos]&.each do |edge|
      queue << Pair.new(edge, d + 1)
    end
  end

  best_from
end

if $0 == __FILE__
  $verbose = false
  puts "going to build graph" if $verbose
  g = Parser.new(File.read(ARGV[0]).strip).build_graph

  room_distances = best_distances(g, Point.new(0, 0))
  p1_ans = room_distances.values.max
  puts "going to build distances from graph" if $verbose
  puts "p1: furthest room is #{p1_ans} moves away"
  p2_ans = room_distances.count { |_pos, dist| dist >= 1_000 }
  puts "p2: #{p2_ans} rooms require at least 1,000 steps"
end
