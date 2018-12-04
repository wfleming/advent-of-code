require "./spec_helper"

describe D3 do
  describe D3::Claim do
    describe "#touches?" do
      it "is correct" do
        claim = D3::Claim.new(1.to_u, {1.to_u, 3.to_u}, 4.to_u, 4.to_u)

        claim.touches?({1.to_u, 3.to_u}).should eq(true)
        claim.touches?({1.to_u, 4.to_u}).should eq(true)
        claim.touches?({4.to_u, 6.to_u}).should eq(true)

        claim.touches?({0.to_u, 4.to_u}).should eq(false)
        claim.touches?({5.to_u, 7.to_u}).should eq(false)
      end
    end

    describe "#all_points" do
      it "works" do
        claim = D3::Claim.new(1.to_u, {1.to_u, 1.to_u}, 2.to_u, 2.to_u)

        claim.all_points.should eq([
          {1.to_u, 1.to_u},
          {1.to_u, 2.to_u},
          {2.to_u, 1.to_u},
          {2.to_u, 2.to_u},
        ])
      end
    end

    describe "overlap?" do
      it "works" do
        claim0 = D3::Claim.new(1.to_u, {1.to_u, 1.to_u}, 2.to_u, 2.to_u)
        claim1 = D3::Claim.new(1.to_u, {2.to_u, 2.to_u}, 1.to_u, 1.to_u)

        claim0.overlap?(claim1).should eq(true)
        claim1.overlap?(claim0).should eq(true)

        claim3 = D3::Claim.new(1.to_u, {10.to_u, 10.to_u}, 5.to_u, 5.to_u)

        claim0.overlap?(claim3).should eq(false)
        claim3.overlap?(claim0).should eq(false)
        claim3.overlap?(claim1).should eq(false)
      end

      it "is correct with real data" do
        claim0 = D3::Claim.new(1.to_u, {906.to_u, 735.to_u}, 28.to_u, 17.to_u)
        claim1 = D3::Claim.new(2.to_u, {189.to_u, 511.to_u}, 10.to_u, 16.to_u)

        claim0.overlap?(claim1).should eq(false)
        claim1.overlap?(claim0).should eq(false)
      end
    end
  end
end
