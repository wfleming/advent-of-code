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
        rule_id = Integer(rule_parts[0])
        rules[rule_id] = Rule.parse(rule_id, rule_parts[1])
      elsif !l.empty?
        messages << l
      end
    end
  end
end

class Rule
  attr_reader :id

  def self.parse(id, str)
    if str.include?("|")
      Or.new(id, str.split("|").map { |r| Rule.parse(nil, r) })
    else
      Seq.new(
        id,
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

  # there could be multiple possible ways to match, so return an array of
  # possibilities
  def match(str, all_rules)
    raise NotImplementeed
  end
end

class Seq < Rule
  attr_reader :symbols

  def initialize(id, symbols)
    @id = id
    @symbols = symbols
  end

  def to_s
    if id.nil?
      "#{symbols.join(" ")}"
    else
      "#{id}: #{symbols.join(" ")}"
    end
  end

  def match(str, all_rules, ident = 0)
    # puts "#{"    " * ident} DEBUG: match rule #{to_s} against #{str}"
    remains = [str]

    symbols.each { |symbol|
      break if remains.empty?

      if symbol.is_a?(String)
        remains = remains.map { |str| str[1..] if str[0] == symbol }.compact
      else
        remains = remains.flat_map { |str|
          all_rules[symbol].match(str, all_rules, ident + 1)
        }
        # puts "#{"    " * ident} DEBUG: after matching #{symbol} remains=#{remains.inspect}"
      end
    }

    remains
  end
end

class Or < Rule
  attr_reader :seqs

  def initialize(id, seqs)
    @id = id
    @seqs = seqs
  end

  def to_s
    "#{id}: #{seqs.map(&:to_s).join(" | ")}"
  end

  def match(str, all_rules, ident = 0)
    # puts "#{"    " * ident} DEBUG: match rule #{to_s} against #{str}"

    # multiple choices could match, i guess, so pick the one that gets us
    # furthest
    seqs.flat_map { |seq| seq.match(str, all_rules, ident + 1) }.
      compact.
      sort_by { |remain| remain.length }
  end
end

class Parser
  attr_reader :message, :rules

  def initialize(message, rules)
    @message = message
    @rules = rules
  end

  def match?
    rules[0].match(message, rules).any? { |s| s == "" }
  end
end

input = Input.new(File.read(ARGV[0]))

valid_count = input.messages.count { |msg| Parser.new(msg, input.rules).match?  }
puts "p1: there are #{valid_count} valid messages"

# p2
p2_rules = input.rules.clone
p2_rules[8] = Or.new(8, [
  Seq.new(nil, [42]),
  Seq.new(nil, [42, 8]),
])
p2_rules[11] = Or.new(11, [
  Seq.new(nil, [42, 31]),
  Seq.new(nil, [42, 11, 31]),
])
valid_count = input.messages.count { |msg| Parser.new(msg, p2_rules).match?  }
#puts "DEBUG: test parse #{p2_rules[0].match("bbbbaabbbbbabbbbbbaabaaabaaa", p2_rules).inspect}"
puts "p2: there are #{valid_count} valid messages"
