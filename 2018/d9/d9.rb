class Game
  attr_reader :players, :high_marble, :active_marble, :current_marble, :marbles, :current_player, :scores

  def initialize(players, high_marble, report_progress: false)
    @players = players
    @high_marble = high_marble

    @current_marble = 0 # "current" according to game rules
    @active_marble = 0  # marble currently being played
    @marbles = [@current_marble]
    @current_player = 0
    @scores = Hash.new(0)

    @report_progress = report_progress
  end

  def run
    until done?
      take_turn
    end
  end

  def done?
    @active_marble == @high_marble
  end

  def take_turn
    if @report_progress && (@active_marble % 5_000 == 0)
      pct = ((@active_marble.to_f / @high_marble) * 100).round(2)
      print "\033[1A\033[" if @progress_printed
      puts "progress #{pct}%"
      @progress_printed = true
    end

    @active_marble += 1
    @current_player += 1
    @current_player = @current_player % @players

    if @active_marble % 23 == 0
      @scores[@current_player] += @active_marble
      remove_idx = (@marbles.index(@current_marble) - 7) % @marbles.length
      @scores[@current_player] += @marbles.delete_at(remove_idx)
      @current_marble = @marbles[remove_idx] # same index works because delete above shifted things
    else
      place_after_idx = @marbles.index(@current_marble) + 1
      place_after_idx = place_after_idx % @marbles.length

      marbles_a = @marbles.shift(place_after_idx + 1)
      @marbles = (marbles_a + [@active_marble] + @marbles)
      @current_marble = @active_marble
    end
  end
end

if $0 == __FILE__
  g = Game.new(ARGV[0].to_i, ARGV[1].to_i, report_progress: true)
  g.run

  puts "p1: high score is #{g.scores.values.max}"
end
