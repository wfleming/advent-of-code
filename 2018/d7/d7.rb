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

if $0 == __FILE__
  dependencies = parse(File.read(ARGV[0]))

  m = Machine.new(dependencies)
  m.run!
  puts "p1: order of steps is #{m.state.done.join("")}"
end
