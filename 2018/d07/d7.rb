# return hash of step -> [dependencies]
def parse(input)
  input.lines.inject(Hash.new) do |memo, line|
    m = /Step (\w) must be finished before step (\w) can begin./.match(line)
    dependency = m[1]
    target = m[2]

    memo[target] ||= []
    memo[target] << dependency

    memo
  end
end

class State
  attr_reader :done

  def initialize(dependencies, done: [], all_steps: nil)
    @dependencies = dependencies
    @done = done
    @all_steps = nil
  end

  def done?
    done.sort == all_steps.sort
  end

  def step
    if next_step.nil?
      raise "can't step, no next step"
    end

    State.new(
      @dependencies,
      done: done << next_step,
      all_steps: all_steps, # small perf optimization
    )
  end

  def next_step
    @next_step ||= available_steps.sort.first
  end

  def available_steps
    # a step is available if it is not done and does not require any undone
    # steps
    all_steps.reject do |s|
      @done.include?(s) || @dependencies.fetch(s, []).any? do |dep|
        !done.include?(dep)
      end
    end
  end

  def all_steps
    @all_steps ||= (@dependencies.keys + @dependencies.values.flat_map { |i| i }).uniq.sort
  end
end

class Machine
  attr_reader :state

  def initialize(deps)
    @state = State.new(deps)
  end

  def run!
    while !@state.done?
      @state = @state.step
    end
  end
end

class State2 < State
  attr_reader :time_elapsed

  def initialize(dependencies, done: [], all_steps: nil, time_elapsed: 0, doing: {}, workers: , base_cost: )
    super(dependencies, done: done, all_steps: all_steps)
    @time_elapsed = time_elapsed
    @workers = workers
    @base_cost = base_cost
    # a hash of step -> time elapsed on it
    @doing = doing
  end

  def step
    next_done = done.dup
    next_doing = @doing.dup

    # start new step(s) if feasible
    if next_doing.count < @workers
      available = available_steps - next_doing.keys
      available.take(@workers - next_doing.count).each do |s|
        next_doing[s] = 0
      end
    end

    # step time on in-progress items
    next_doing.each do |s, t|
      next_doing[s] = t + 1
    end

    # # determine what just finished
    just_done = next_doing.select do |s, t|
      t >= step_time(s)
    end.keys

    # # if things finished, move out of doing, onto done
    next_doing.reject! { |s, _| just_done.include?(s) }
    next_done += just_done

    State2.new(
      @dependencies,
      time_elapsed: @time_elapsed + 1,
      done: next_done,
      doing: next_doing,
      workers: @workers,
      base_cost: @base_cost,
      all_steps: all_steps, # small perf optimization
    )
  end

  def step_time(step)
    @step_names ||= ('A'..'Z').to_a

    @base_cost + @step_names.index(step) + 1
  end
end

class Machine2
  attr_reader :state

  def initialize(deps, workers, base_cost)
    @state = State2.new(deps, workers: workers, base_cost: base_cost)
  end

  def run!
    while !@state.done?
      @state = @state.step
    end
  end
end

if $0 == __FILE__
  dependencies = parse(File.read(ARGV[0]))

  m = Machine.new(dependencies)
  m.run!
  puts "p1: order of steps is #{m.state.done.join("")}"

  m2 = Machine2.new(dependencies, 5, 60)
  m2.run!
  puts "p2: time take is #{m2.state.time_elapsed}"
end
