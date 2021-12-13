require "../spec_helper"

describe D3::Overlap do
  it "counts overlaps" do
      claims = [
        D3::Claim.new(1.to_u, {1.to_u, 3.to_u}, 4.to_u, 4.to_u),
        D3::Claim.new(2.to_u, {3.to_u, 1.to_u}, 4.to_u, 4.to_u),
        D3::Claim.new(3.to_u, {5.to_u, 5.to_u}, 2.to_u, 2.to_u),
      ]
      overlap = D3::Overlap.new(claims)

      overlap.overlap_count.should eq(4)
  end
end
