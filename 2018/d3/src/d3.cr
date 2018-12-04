require "../lib/d3.cr"

input = ARGF.gets_to_end
parser = D3::Parser.new

claims = parser.parse(input)

overlap_count = D3::Overlap.new(claims).overlap_count
puts "p1: overlap of #{overlap_count}"

solo_claim = claims.find do |claim|
  claims.none? do |other_claim|
    other_claim.id != claim.id &&
      claim.overlap?(other_claim)
  end
end

if solo_claim.nil?
  puts "p2: no match found"
else
  puts "p2: id of claim without overlap = #{solo_claim.id}"
end
