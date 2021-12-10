#!/usr/bin/env ruby

Error = Struct.new(:found, :expected)
ErrorResult = Struct.new(:errors, :open_stack)

PAIRS = { "(" => ")", "{" => "}", "[" => "]", "<" => ">"}
ERR_SCORES = { ")" => 3, "]" => 57, "}" => 1197, ">" => 25137 }
COMPLETION_SCORES = { ")" => 1, "]" => 2, "}" => 3, ">" => 4 }

# return { errors: [], open_stack: [] }
def find_errors(line)
  open_stack = []
  errors = []

  line.each_char do |char|
    if PAIRS.include?(char) # new open
      open_stack.push(char)
    elsif char != PAIRS.fetch(open_stack[-1]) # invalid close
      open = open_stack.pop
      errors << Error.new(char, PAIRS[open])
    else # valid close
      open_stack.pop
    end
  end

  ErrorResult.new(errors, open_stack)
end

# return nil if line is corrupt, array of chars otherwise
def auto_complete(line)
  errs_and_open_stack = find_errors(line)

  return nil if errs_and_open_stack.errors.any?

  errs_and_open_stack.open_stack.reverse.map { |c| PAIRS.fetch(c) }
end

def score_completion(completions)
  completions.reduce(0) { |memo, char| (memo * 5) + COMPLETION_SCORES.fetch(char) }
end

lines = File.readlines(ARGV[0]).map(&:chomp)
first_errs = lines.map(&method(:find_errors)).map(&:errors).select(&:any?).map(&:first)
score = first_errs.sum { |e| ERR_SCORES.fetch(e.found) }
puts "p1: score = #{score}"

completions = lines.map(&method(:auto_complete)).compact
scores = completions.map(&method(:score_completion))
raise StandardError, "Should be an odd number of completions" unless scores.count.odd?
winning_score = scores.sort[(scores.count - 1) / 2]
puts "p2: winning score = #{winning_score}"
