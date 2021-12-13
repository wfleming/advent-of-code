require "../spec_helper"

describe D3::Parser do
  it "works" do
    input = "#1 @ 1,3: 4x4\n" +
            "#2 @ 3,1: 4x4\n" +
            "#3 @ 5,5: 2x2\n"

    claims = D3::Parser.new.parse(input)

    claims.should eq([
      D3::Claim.new(1.to_u, {1.to_u, 3.to_u}, 4.to_u, 4.to_u),
      D3::Claim.new(2.to_u, {3.to_u, 1.to_u}, 4.to_u, 4.to_u),
      D3::Claim.new(3.to_u, {5.to_u, 5.to_u}, 2.to_u, 2.to_u),
    ])
  end
end
