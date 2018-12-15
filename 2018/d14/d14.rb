SEED_SCORES = [3, 7]
ELF_COUNT = 2

$state_score_buffer = 30_000_000

class State
  attr_reader :scores, :score_count, :elves, :ticks

  def initialize(init_scores, elves_count)
    @scores = init_scores + ([nil] * $state_score_buffer)
    @score_count = init_scores.count
    @elves = elves_count.times.to_a
    @ticks = 0
  end

  def tick
    @ticks += 1

    sum = elves.map do |current_recipe_idx|
      scores[current_recipe_idx]
    end.sum
    new_recipes = sum.to_s.each_char.map(&:to_i)
    new_recipes.each do |s|
      @scores[score_count] = s
      @score_count += 1
    end

    @elves = elves.map do |current_recipe_idx|
      (current_recipe_idx + 1 + scores[current_recipe_idx]) % score_count
    end
  end
end

def p1(state, after_count)
  until state.score_count > after_count + 10
    state.tick
  end

  state.scores[after_count, 10]
end

def find_seq(state, search)
  # search backwards, never search more than 2x length of search string,
  # since earlier bits of array don't change
  idx = state.score_count - search.length
  while idx >= 0 && idx > idx - (2 * search.length)
    if state.scores[idx, search.length].join("") == search
      return idx
    end
    idx -= 1
  end
  nil
end

def p2(state, search, print_progress: false)
  progress_printed = false

  found_idx = nil
  until (found_idx = find_seq(state, search))
    state.tick

    if print_progress
      print "\033[1A\033[" if progress_printed
      progress_printed = true
      puts "_ ticked #{state.ticks} times (have #{state.score_count} recipes)"
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
