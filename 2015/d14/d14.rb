#!/usr/bin/env ruby

class Reindeer
  PAT = %r{(?<name>\w+) can fly (?<speed>\d+) km/s for (?<stamina>\d+) seconds, but then must rest for (?<refractory>\d+) seconds\.}

  attr_reader :name, :current_state, :state_duration, :km_flown
  attr_accessor :secs_in_lead

  def self.parse(line)
    m = PAT.match(line)
    self.new(m["name"], Integer(m["speed"]), Integer(m["stamina"]), Integer(m["refractory"]))
  end

  def initialize(name, speed, stamina, refractory_period)
    @name = name
    @speed = speed
    @stamina = stamina
    @refractory_period = refractory_period
    @current_state = :flying
    @state_duration = 0
    @km_flown = 0
    @secs_in_lead = 0
  end

  def tick
    if @current_state == :flying
      @km_flown += @speed
      @state_duration += 1
      if @state_duration == @stamina
        @current_state = :resting
        @state_duration = 0
      end
    elsif @current_state == :resting
      @state_duration += 1
      if @state_duration == @refractory_period
        @current_state = :flying
        @state_duration = 0
      end
    end
  end
end

RACE_LENGTH = 2503 # 2503 for real, 1000 for sample data

reindeer = File.readlines(ARGV[0]).map(&Reindeer.method(:parse))
RACE_LENGTH.times do
  reindeer.map(&:tick)
  lead_distance = reindeer.map(&:km_flown).max
  reindeer.each do |r|
    r.secs_in_lead += 1 if r.km_flown == lead_distance
  end
end

p1_winner = reindeer.sort_by { |r| 0 - r.km_flown }[0]
puts "p1: #{p1_winner.name} is in the lead at #{p1_winner.km_flown} km"

p2_winner = reindeer.sort_by { |r| 0 - r.secs_in_lead }[0]
puts "p2: #{p2_winner.name} is in the lead with score of #{p2_winner.secs_in_lead}"
