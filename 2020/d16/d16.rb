#!/usr/bin/env ruby

CONSTRAINT_PAT = /^([\w ]+): \d/
RANGE_PAT = /(\d+)-(\d+)/

Input = Struct.new(:constraints, :my_ticket, :other_tickets) do
  def self.parse(str)
    constraints = Hash.new
    my_ticket = nil
    other_tickets = []
    mode = :constraints

    str.each_line do |line|
      if (m = CONSTRAINT_PAT.match(line))
        field = m[1]
        ranges = line.scan(RANGE_PAT).map do |match|
          Integer(match[0])..Integer(match[1])
        end

        constraints[field] = ranges
      elsif line.strip == "your ticket:"
        mode = :my_ticket
      elsif line =~ /^\d/ && mode == :my_ticket
        my_ticket = line.strip.split(",").map(&method(:Integer))
      elsif line.strip == "nearby tickets:"
        mode = :nearby_tickets
      elsif line =~ /^\d/ && mode == :nearby_tickets
        other_tickets << line.strip.split(",").map(&method(:Integer))
      end
    end

    self.new(constraints, my_ticket, other_tickets)
  end

  # p1 answer
  def scanning_error_rate
    other_tickets.lazy.map { |ticket|
      # find the invalid value
      ticket.find { |x| constraints.values.flatten.none? { |r| r === x } }
    }.reject(&:nil?).sum
  end
end

input = Input.parse(File.read(ARGV[0]))
#puts "DEBUG: input = #{input}"

puts "p1: ticket scanning error rate = #{input.scanning_error_rate}"

