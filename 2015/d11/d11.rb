#!/usr/bin/env ruby

class Validator
  def initialize(password)
    @password = password
  end

  def valid?
    !banned_chars? && straight? && pairs?
  end

  def straight?
    @password.chars.each_with_index do |char, idx|
      char_1 = @password[idx + 1]
      char_2 = @password[idx + 2]
      if char && char_1 && char_2 && char.ord + 1 == char_1.ord &&
          char_1.ord + 1 == char_2.ord
        return true
      end
    end
    false
  end

  def banned_chars?
    /[iol]/.match?(@password)
  end

  def pairs?
    # each entry is [char, idx]
    pairs = []

    @password.chars.each_with_index do |char, idx|
      next unless @password[idx + 1] == char

      if pairs.empty? || pairs[-1][1] < idx - 1
        pairs << [char, idx]
      end
    end

    pairs.count > 1
  end
end

def next_password(password)
  if password == ""
    ""
  elsif password[-1] == "z"
    next_password(password[0..-2]) + "a"
  else
    password = password[0..-2] + (password[-1].ord + 1).chr
  end
end

def next_valid_password(password)
  p = next_password(password)
  until Validator.new(p).valid?
    p = next_password(p)
  end
  p
end

if __FILE__ == $0
  p1_pass = next_valid_password(ARGV[0])
  puts "p1: next password is #{p1_pass}"

  p2_pass = next_valid_password(p1_pass)
  puts "p2: next password is #{p2_pass}"
end
