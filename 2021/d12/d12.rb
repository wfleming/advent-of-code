#!/usr/bin/env ruby

require "set"

def parse_edges(str)
  str.lines.map { |l| l.strip.split("-") }
end

class QueueSet
  def initialize(items = [])
    @queue = items
    @membership = Set.new(items)
  end

  def shift
    @queue.shift.tap { |i| @membership.delete(i) }
  end

  def push(item)
    unless include?(item)
      @queue.push(item)
      @membership << item
    end
  end

  def count; @membership.count; end
  def any?; count > 0; end
  def include?(item); @membership.include?(item); end
end

def next_steps(path, edges, max_small_revisits)
  next_steps = edges[path[-1]]
  small_cave_revisits = path.uniq.count { |n| n =~ /[a-z]/ && path.count(n) > 1 }
  # remove "small" nodes already visited if we've hit our limit
  if small_cave_revisits == max_small_revisits
    next_steps = next_steps.reject { |node| node =~ /[a-z]/ && path.include?(node) }
  end
  next_steps
end

def find_paths_bfs(edges, max_small_revisits=0)
  # since we have to find every path no need to think about priority/order,
  # but the open set could get large so more efficient membership checks might
  # be useful
  open_set = QueueSet.new([["start"]])
  complete_set = Set.new

  while open_set.any?
    # puts "DEBUG: open_set=#{open_set.count} complete_set=#{complete_set.count}"
    path = open_set.shift

    if path[-1] == "end"
      complete_set << path
      next
    end

    next_paths = next_steps(path, edges, max_small_revisits).map { |node| path + [node] }
    next_paths.each { |np| open_set.push(np) unless open_set.include?(np) }
  end

  complete_set
end

def find_paths_dfs(edges, max_small_revisits=0, path=["start"])
  if path[-1] == "end"
    [path]
  else
    next_paths = next_steps(path, edges, max_small_revisits).map { |node| path + [node] }
    next_paths.flat_map { |p| find_paths_dfs(edges, max_small_revisits, p) }
  end
end

edges = parse_edges(File.read(ARGV[0])).reduce(Hash.new([])) do |memo, edge|
  memo[edge[0]] += [edge[1]]
  memo[edge[1]] += [edge[0]]
  memo
end.transform_values { |ns| ns - ["start" ] }.tap { |h| h.delete("end") }

# all_paths = find_paths_bfs(edges)
# puts "p1: there are #{all_paths.count} paths through the caves"
all_paths = find_paths_dfs(edges)
puts "p1 (dfs): there are #{all_paths.count} paths through the caves"

all_paths_p2 = find_paths_dfs(edges, 1)
puts "p2: there are #{all_paths_p2.count} paths through the caves"
