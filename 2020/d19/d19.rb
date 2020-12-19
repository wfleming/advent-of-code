#!/usr/bin/env ruby

class Input
  attr_reader :rules, :messages

  def initialize(str)
    @rules = {}
    @messages = []

    extract(str)
  end

  def extract(str)
    str.each_line do |l|
      l = l.strip

      if /^\d+:/ =~ l
        rule_parts = l.split(":")
        rules[Integer(rule_parts[0])] = Rule.parse(rule_parts[1])
      elsif !l.empty?
        messages << l
      end
    end
  end
end

class Rule
  def self.parse(str)
    if str.include?("|")
      Or.new(str.split("|").map { |r| Rule.parse(r) })
    else
      Seq.new(
        str.split(" ").map { |tok|
          if /\d+/ =~ tok
            Integer(tok)
          elsif /"[ab]"/ =~ tok
            tok[1]
          else
            raise "unexpected token #{tok}"
          end
        }
      )
    end
  end

  # consume as far as possible, return remaining un-matched str or nil if no
  # match
  def match(str, all_rules)
    raise NotImplementeed
  end
end

class Seq < Rule
  attr_reader :symbols

  def initialize(symbols)
    @symbols = symbols
  end

  def match(str, all_rules)
    remain = str

    symbols.each { |symbol|
      break if remain.nil?

      if symbol.is_a?(String)
        if remain[0] == symbol
          remain = remain[1..]
        else
          remain = nil
        end
      else
        remain = all_rules[symbol].match(remain, all_rules)
      end
    }

    remain
  end
end

class Or < Rule
  attr_reader :seqs

  def initialize(seqs)
    @seqs = seqs
  end

  def match(str, all_rules)
    # multiple choices could match, i guess, so pick the one that gets us
    # furthest
    seqs.map { |seq| seq.match(str, all_rules) }.
      compact.
      sort_by { |remain| remain.length }.
      first
  end
end

class Parser
  attr_reader :message, :rules

  def initialize(message, rules)
    @message = message
    @rules = rules
  end

  def match?
    rules[0].match(message, rules) == ""
  end
end

input = Input.new(File.read(ARGV[0]))
# puts "DEBUG input = #{input.inspect}"
# input.messages.each do |msg|
#   parser = Parser.new(msg, input.rules)
#   puts "DEBUG: msg='#{msg}' matches_grammar=#{parser.match?} remain=#{input.rules[0].match(msg, input.rules).inspect}"
# end
valid_count = input.messages.count { |msg| Parser.new(msg, input.rules).match?  }
puts "p1: there are #{valid_count} valid messages"

