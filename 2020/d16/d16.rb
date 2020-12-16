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

  def invalid?(ticket)
    ticket.any? { |x| constraints.values.flatten.none? { |r| r === x } }
  end

  def valid_tickets
    @valid_tickets ||= other_tickets.reject(&method(:invalid?))
  end

  # return [string] (first entry is the name of the first field on each ticket)
  def mapped_fields
    field_names = [nil] * other_tickets[0].count

    while field_names.any?(&:nil?)
      i = field_names.each_index.find { |idx|
        field_names[idx].nil? && identify_field(idx, field_names.compact).count == 1
      }
      field_names[i] = identify_field(i, field_names.compact).first
    end

    field_names
  end

  # returns the name
  def identify_field(idx, exclude_names = [])
    vals = valid_tickets.map { |t| t[idx] }
    constraints.select { |name, ranges|
      !exclude_names.include?(name) && vals.all? { |v| ranges.any? { |r| r === v } }
    }.keys
  end

  def my_departure_fields
    field_names = mapped_fields
    my_ticket.each_with_index.map { |val, idx|
      val if field_names[idx] =~ /^departure/
    }.compact
  end
end

input = Input.parse(File.read(ARGV[0]))
#puts "DEBUG: input = #{input}"

puts "p1: ticket scanning error rate = #{input.scanning_error_rate}"

my_departure_fields = input.my_departure_fields
puts "p2: my fields = #{my_departure_fields}, product = #{my_departure_fields.reduce(&:*)}"
