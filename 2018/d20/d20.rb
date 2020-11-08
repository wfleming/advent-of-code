require "pry" # DEBUG
require "pqueue"
require "forwardable"

Point = Struct.new(:x, :y)

def pt(x, y)
  Point.new(x, y)
end

class Graph
  include Enumerable
  extend Forwardable

  Node = Struct.new(:id, :edges)

  attr_reader :vertices

  def_delegators :@vertices, :each

  def initialize(root)
    @vertices = [root]
  end

  def add_edge(id_a, id_b, reverse: false)
    find_node(id_a).edges << find_or_build_node(id_b)
    if reverse
      find_node(id_b).edges << find_node(id_a)
    end
  end

  def find_or_build_node(id)
    find_node(id) || Node.new(id, []).tap { |n| @vertices << n }
  end

  def find_node(id)
    find { |n| n.id == id }
  end
end

class Parser
  attr_reader :input

  def initialize(str)
    @input = str.each_char.to_a
  end

  def build_graph
    r = Graph::Node.new(pt(0, 0), [])
    g = Graph.new(r)

    input.shift if input[0] == "^"
    walk(g, r.id)

    g
  end

  def walk(graph, root_id)
    current_id = root_id
    while (c = input.shift)
      if $verbose
        print "\033[1A\033[" if @progress_printed
        @progress_printed = true
        puts "_ graph has #{graph.count} nodes"
      end
      next_pt =
        case c
        when "N"
          current_id = pt(current_id.x, current_id.y - 1).tap do |pt2|
            graph.add_edge(current_id, pt2)
          end
        when "S"
          current_id = pt(current_id.x, current_id.y + 1).tap do |pt2|
            graph.add_edge(current_id, pt2)
          end
        when "W"
          current_id = pt(current_id.x - 1, current_id.y).tap do |pt2|
            graph.add_edge(current_id, pt2)
          end
        when "E"
          current_id = pt(current_id.x + 1, current_id.y).tap do |pt2|
            graph.add_edge(current_id, pt2)
          end
        when "(" # push onto stack
          walk(graph, current_id)
        when "|" # alt route
          walk(graph, root_id)
        when ")" # pop stack
          break
        when "$" # end of the regex
          break
        end
    end
  end
end

def viz(graph)
  min_x = graph.map(&:id).map(&:x).min
  max_x = graph.map(&:id).map(&:x).max
  min_y = graph.map(&:id).map(&:y).min
  max_y = graph.map(&:id).map(&:y).max

  width = (1 + max_x - min_x) * 2 + 1

  lines = ["#" * width]

  (min_y..max_y).each do |y|
    # line1 contains this y, line2 contains walls & doors to y + 1
    line1 = "#"
    line2 = "#"
    (min_x..max_x).each do |x|
      p = pt(x, y)
      n = graph.find_node(p)

      if n
        line1 << (p == pt(0,0) ? "X" : ".")
        right = pt(x + 1, y)
        if n.edges.find { |n2| n2.id == right } || graph.find_node(right)&.edges&.map(&:id)&.include?(p)
          line1 << "|"
        else
          line1 << "#"
        end
        down = pt(x, y + 1)
        if n.edges.find { |n2| n2.id == down } || graph.find_node(down)&.edges&.map(&:id)&.include?(p)
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

def best_distances(graph, start)
  progress_printed = false

  queue = PQueue.new # [pt, dist]
  queue << Pair.new(start, 0)
  best_from = Hash.new # node_id -> dist

  while queue.size > 0
    pair = queue.shift
    n, d = pair.id, pair.dist

    next if best_from.key?(n) && best_from[n] <= d

    if $verbose
      print "\033[1A\033[" if progress_printed
      progress_printed = true
      puts "_ searching seen #{best_from.count}, #{queue.size} in queue"
    end

    best_from[n] = d

    graph.find_node(n).edges.each do |edge|
      queue << Pair.new(edge.id, d + 1)
    end
  end

  best_from
end

def p1(g)
  distances = best_distances(g, pt(0, 0))
  distances.values.max
end

if $0 == __FILE__
  $verbose = true
  puts "going to build graph"
  g = Parser.new(File.read(ARGV[0]).strip).build_graph

  puts "going to build distances from graph"
  puts "p1: furthest room is #{p1(g)} moves away"
end
