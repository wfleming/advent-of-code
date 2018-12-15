SEED_SCORES = [3, 7]
ELF_COUNT = 2

class State
  attr_reader :scores, :elves, :ticks

  def initialize(scores, elves_count)
    @scores = scores
    @elves = elves_count.times.to_a
    @ticks = 0
  end

  def tick
    @ticks += 1

    sum = elves.map do |current_recipe_idx|
      scores[current_recipe_idx]
    end.sum
    new_recipes = sum.to_s.each_char.map(&:to_i)
    @scores += new_recipes

    @elves = elves.map do |current_recipe_idx|
      (current_recipe_idx + 1 + scores[current_recipe_idx]) % scores.length
    end
  end
end

def p1(state, after_count)
  until state.scores.length > after_count + 10
    state.tick
  end

  state.scores[after_count, 10]
end

def find_seq(scores, search)
  # search backwards in-place for performance
  idx = scores.count - search.length
  while idx >= 0
    if scores[idx, search.length].join("") == search
      return idx
    end
    idx -= 1
  end
  nil
end

def p2(state, search, print_progress: false, initial_run: 10_000_000)
  progress_printed = false

  # large initial run to generate without wasting time searching
  initial_run.times do
    state.tick

    if print_progress && state.ticks % 10_000 == 0
      print "\033[1A\033[" if progress_printed
      progress_printed = true
      puts "_ ticked #{state.ticks} times"
    end
  end

  found_idx = nil
  until (found_idx = find_seq(state.scores, search))
    1_000_000.times { state.tick }

    if print_progress
      print "\033[1A\033[" if progress_printed
      progress_printed = true
      puts "_ ticked #{state.ticks} times (have #{state.scores.count} recipes)"
    end
  end

  all_scores = state.scores.join("")
  all_scores.index(search)
end

if $0 == __FILE__
  state1 = State.new(SEED_SCORES, ELF_COUNT)

  # p1_result = p1(state1, ARGV[0].to_i)
  # puts "p1 result = #{p1_result.join("")}"

  state2 = State.new(SEED_SCORES, ELF_COUNT)
  puts "p1 result = #{p2(state2, ARGV, print_progress: true)}"
end
