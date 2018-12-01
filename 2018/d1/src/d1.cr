require "./parser"

input = ARGF.gets_to_end
steps = Parser.new.parse(input)

p1_freq = steps.sum
puts "p1: frequency ends at #{p1_freq}"

seen_freqs = Set.new [0]
current_freq = 0
first_repeat = nil
while first_repeat.nil?
  steps.each do |freq|
    current_freq = current_freq + freq

    if seen_freqs.includes?(current_freq)
      first_repeat = current_freq
      break
    else
      seen_freqs << current_freq
    end
  end
end

puts "p2: first repeated frequency is #{first_repeat}"
