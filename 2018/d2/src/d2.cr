require "./checksum"
require "./matcher"

ids = ARGF.gets_to_end.lines.map { |l| l.strip }

p1_checksum = Checksum.new(ids).checksum
puts "p1: list checksum is #{p1_checksum}"

matcher = Matcher.new(ids)
puts "p2: matched ids are '#{matcher.matched_ids}'"
puts "p2: shared chars are '#{matcher.shared_chars}'"

