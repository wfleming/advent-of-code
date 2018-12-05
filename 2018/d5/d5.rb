def collapse(polymer)
  p2 = polymer.dup

  ('a'..'z').each do |char|
    pat1 = /#{char}#{char.upcase}/
    pat2 = /#{char.upcase}#{char}/

    p2 = p2.gsub(pat1, "")
    p2 = p2.gsub(pat2, "")
  end

  if p2.length == polymer.length
    p2
  else
    collapse(p2)
  end
end

def most_efficient(polymer)
  h = {}

  ('a'..'z').each do |c|
    p2 = polymer.gsub(/#{c}/i, '')
    h[c] = collapse(p2).length
  end

  min_l = h.values.min
  h.detect { |k, v| v == min_l }
end

if $0 == __FILE__
  input = File.read(ARGV[0]).strip

  p1_collapsed = collapse(input)

  puts "p1: original had #{input.length} units, collapsed has #{p1_collapsed.length}"

  p2_winner = most_efficient(input)

  puts "p2: best removal was #{p2_winner[0]} units, resulted in length of #{p2_winner[1]}"
end
