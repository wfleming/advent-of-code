require "minitest/autorun"

require "d20"

describe Parser do
  it "parses a simple regex" do
    p = Parser.new("^NWS$")
    g = p.build_graph

    g.count.must_equal 4
  end

  it "parses branches" do
    p = Parser.new("^ENWWW(NEEE|SSE(EE|N))$")
    g = p.build_graph

    viz(g).must_equal <<~GRAPH.chomp
    #########
    #.|.|.|.#
    #-#######
    #.|.|.|.#
    #-#####-#
    #.#.#X|.#
    #-#-#####
    #.|.|.|.#
    #########
    GRAPH
  end

  it "parses branches with empty option" do
    p = Parser.new("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")
    g = p.build_graph

    viz(g).must_equal <<~GRAPH.chomp
    ###########
    #.|.#.|.#.#
    #-###-#-#-#
    #.|.|.#.#.#
    #-#####-#-#
    #.#.#X|.#.#
    #-#-#####-#
    #.#.|.|.|.#
    #-###-###-#
    #.|.|.#.|.#
    ###########
    GRAPH
  end

  it "parses a complex graph" do
    # goes haywire at the first branch, going from NW(
    p = Parser.new("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")
    g = p.build_graph

    File.open("viz", "w") { |f| f.write(viz(g)) }
    viz(g).must_equal <<~GRAPH.chomp
    ###############
    #.|.|.|.#.|.|.#
    #-###-###-#-#-#
    #.|.#.|.|.#.#.#
    #-#########-#-#
    #.#.|.|.|.|.#.#
    #-#-#########-#
    #.#.#.|X#.|.#.#
    ###-#-###-#-#-#
    #.|.#.#.|.#.|.#
    #-###-#####-###
    #.|.#.|.|.#.#.#
    #-#-#####-#-#-#
    #.#.|.|.|.#.|.#
    ###############
    GRAPH
  end
end

describe "#viz" do
  it "visualizes a simple graph" do
    p = Parser.new("^WNE$")
    g = p.build_graph

    viz(g).must_equal <<~GRAPH.chomp
    #####
    #.|.#
    #-###
    #.|X#
    #####
    GRAPH
  end
end

describe "#p1" do
  it "works on a simple graph" do
    g = Parser.new("^WNE$").build_graph
    p1(g).must_equal 3
  end

  it "works on a more complex graph" do
    g = Parser.new("^ENWWW(NEEE|SSE(EE|N))$").build_graph
    p1(g).must_equal 10
  end

  it "works on an even more complex graph" do
    g = Parser.new("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$").build_graph
    p1(g).must_equal 18
  end

  it "works on an another even more complex graph" do
    g = Parser.new("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$").build_graph
    p1(g).must_equal 31
  end
end
