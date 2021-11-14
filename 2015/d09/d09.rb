#!/usr/bin/env ruby

require "set"

Vertex = Struct.new(:from, :to, :distance) do
  PAT = /(\w+) to (\w+) = (\d+)/
  def self.parse(line)
    m = PAT.match(line)
    return self.new(m[1], m[2], Integer(m[3]))
  end

  def self.parse_all(lines)
    lines.map(&method(:parse))
  end

  def reversed
    self.class.new(to, from, distance)
  end
end

class Route
  attr_reader :nodes

  def initialize(nodes)
    @nodes = nodes
  end

  def current_city
    nodes[-1].to
  end

  def <<(node)
    @nodes << node
  end

  def cost
    nodes.map(&:distance).sum
  end

  def cities_visited
    (nodes.map(&:from) + nodes.map(&:to)).uniq.sort
  end

  def to_s
    cities = [nodes.first.from] + nodes.map(&:to)
    return "<Route #{cities.join(" -> ")} (#{cost})>"
  end
end

class RouteBuilder
  attr_reader :all_vertices, :all_cities

  def initialize(vertices)
    @all_vertices = vertices
    @all_cities = Set.new(vertices.map(&:from) + vertices.map(&:to))
  end

  # traveling salesman is NP-hard, but p1 looks small enough to brute force
  def all_routes
    seeds = all_vertices.flat_map { |v| [Route.new([v]), Route.new([v.reversed])] }
    extend_routes(seeds)
  end

  def extend_routes(routes)
    # puts "DEBUG extend_routes (currently #{routes.count} routes)"
    next_routes = routes.flat_map do |r|
      remaining_cities = all_cities - r.cities_visited
      if remaining_cities.empty?
        r
      else
        eligible_next_steps = all_vertices.select do |v|
          [v.from, v.to].include?(r.current_city) && (
            remaining_cities.include?(v.from) || remaining_cities.include?(v.to)
          )
        end
        if eligible_next_steps.empty?
          puts "DEBUG: dead-end route #{r}"
          nil
        else
          eligible_next_steps.map do |v|
            n = v.from == r.current_city ? v : v.reversed
            Route.new(r.nodes + [n])
          end
        end
      end
    end.compact

    if next_routes.all? { |r| Set.new(r.cities_visited) == all_cities }
      next_routes
    else
      extend_routes(next_routes)
    end
  end
end

vertices = Vertex.parse_all(File.readlines(ARGV[0]))

routes = RouteBuilder.new(vertices).all_routes.sort_by(&:cost)
puts "p1: best route is #{routes.first}, costs #{routes.first.cost}"
puts "p2: best route is #{routes.last}, costs #{routes.last.cost}"
