# doubly-linked and circular linked list
class Node
  attr_accessor :left, :val, :right

  def self.create_list(val)
    Node.new(nil, val, nil).tap do |n|
      n.left = n
      n.right = n
    end
  end

  def initialize(left, val, right)
    @left = left
    @val = val
    @right = right
  end

  def insert_before(new_val)
    Node.new(left, new_val, self).tap do |n|
      # our old left's new right is the new node
      left.right = n
      # our new left is the new node
      self.left = n
    end
  end

  def remove
    left.right = right
    right.left = left

    right
  end

  def inspect
    "<Node left=#{left&.val} val=#{val} right=#{right&.val}>"
  end

  def to_a
    x = self
    vs = [x.val]
    while x.right != self
      x = x.right
      vs << x.val
    end
    vs
  end
end

class Game
  attr_reader :players, :high_marble, :active_marble, :current_marble, :marbles, :current_player, :scores

  def initialize(players, high_marble, report_progress: false)
    @players = players
    @high_marble = high_marble

    @active_marble = 0  # marble currently being played
    @current_marble = Node.create_list(0) # "current" according to game rules
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

      remove_target = @current_marble
      7.times { remove_target = remove_target.left }

      @scores[@current_player] += remove_target.val
      @current_marble = remove_target.remove
    else
      @current_marble = @current_marble.right.right.insert_before(@active_marble)
    end
  end
end

if $0 == __FILE__
  g = Game.new(ARGV[0].to_i, ARGV[1].to_i, report_progress: true)
  g.run

  puts "p1: high score is #{g.scores.values.max}"

  g2 = Game.new(ARGV[0].to_i, ARGV[1].to_i * 100, report_progress: true)
  g2.run

  puts "p2: high score is #{g2.scores.values.max}"
end
