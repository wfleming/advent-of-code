#!/usr/bin/env ruby

require_relative "../lib/pqueue"

def parse_input
  lines = File.readlines(ARGV[0])

  transforms = lines.inject({}) do |memo, l|
    if (m = /^(\w+) => (\w+)/.match(l))
      memo[m[1]] ||= []
      memo[m[1]] << m[2]
    end
    memo
  end

  # transforms is hash of { source => [possible transforms] }
  return [lines[-1].chomp, transforms]
end

def available_transforms(molecule, transforms)
  transforms.flat_map do |source, replacements|
    new_molecules = []
    search_offset = 0
    while (i = molecule.index(source, search_offset))
      new_molecules += replacements.map do |replacement|
        left = molecule.chars.take(i).join("")
        right = molecule.chars.drop(i + source.length).join("")
        "#{left}#{replacement}#{right}"
      end
      search_offset = i + 1
    end
    new_molecules
  end.uniq
end

# https://stackoverflow.com/questions/46402903/levenshtein-distance-in-ruby
def levenshtein_distance(s, t)
  v0 = (0..t.length).to_a
  v1 = []

  s.chars.each_with_index do |s_ch, i|
    v1[0] = i + 1

    t.chars.each_with_index do |t_ch, j|
      cost = s_ch == t_ch ? 0 : 1
      v1[j + 1] = [v1[j] + 1, v0[j + 1] + 1, v0[j] + cost].min
    end
    v0 = v1.dup
  end

  v0[t.length]
end

# https://en.wikipedia.org/wiki/A*_search_algorithm
def astar_search(start, goal, transforms)
  open_set = PQueue.new()
  open_set.push(start, levenshtein_distance(start, goal))

  came_from = {} # molecule => prev_molecule
  g_scores = {start => 0} # molecule => # transforms to get there
  f_scores = {start => levenshtein_distance(start, goal)}

  # i = 0
  # loop_times, sort_times = [], []
  while open_set.any?
    n = open_set.shift
    # if i % 100 == 0 # DEBUG
    #   puts "DEBUG: avg loop time = #{loop_times.sum / loop_times.count} avg sort time = #{sort_times.sum / sort_times.count }" if loop_times.count > 0
    #   puts "DEBUG: current estimated distance to goal = #{f_scores[n]}, open_set.count=#{open_set.count}"
    #   loop_times, sort_times = [], []
    # end
    # t = Process.clock_gettime(Process::CLOCK_MONOTONIC) # DEBUG
    # i += 1 # DEBUG
    if n == goal
      path = [n]
      while came_from.include?(n)
        n = came_from[n]
        path.unshift(n)
      end
      return path
    end
    available_transforms(n, transforms).each do |next_molecule|
      g_score = g_scores[n] + 1
      if g_score < g_scores.fetch(next_molecule, Float::INFINITY)
        came_from[next_molecule] = n
        g_scores[next_molecule] = g_score
        f_scores[next_molecule] = g_score + levenshtein_distance(next_molecule, goal)
        # Just realized this may be a bug I got lucky with? Shouldn't A* update
        # the priority in the queue if it's changed even if the pqueue already
        # has the item?
        open_set.push(next_molecule, f_scores[next_molecule]) unless open_set.include?(next_molecule)
      end
    end
    # st = Process.clock_gettime(Process::CLOCK_MONOTONIC) # DEBUG
    # open_set.sort_by! { |s| f_scores[s] }
    # et = Process.clock_gettime(Process::CLOCK_MONOTONIC) # DEBUG
    # loop_times << et - t # DEBUG
    # sort_times << et - st # DEBUG
  end

  raise StandardError, "Could not find path to goal"
end

medicine_molecule, transforms = *parse_input

potential_next_molecules = available_transforms(medicine_molecule, transforms)
puts "p1: #{potential_next_molecules.count} potential molecules can be created with 1 transform"

# backwards search is considerably more efficient
inverted_transforms = transforms.inject({}) do |memo, replacement_pair|
  replacement_source, replacements = replacement_pair
  replacements.each do |replacement|
    memo[replacement] ||= []
    memo[replacement] << replacement_source
  end
  memo
end
p2_steps = astar_search(medicine_molecule, "e", inverted_transforms)
puts "p2: #{p2_steps.count - 1} is the minimum number of transforms"
