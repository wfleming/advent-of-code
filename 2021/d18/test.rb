#!/usr/bin/env ruby

require "minitest/autorun"
require_relative "d18"

describe Tree do
  describe :to_a do
    it "should equal the input to treeify" do
      sn = [[[[[9,8],1],2],3],4]
      t = Tree.treeify(sn)
      _(t.to_a).must_equal(sn)
    end
  end

  describe :clone do
    it "should produce the same object" do
      t = Tree.treeify([[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]])
      _(t.clone.to_a).must_equal(t.to_a)
    end
  end

  describe :explode do
    it "works for [[[[[9,8],1],2],3],4]" do
      t = Tree.treeify([[[[[9,8],1],2],3],4])
      n = t.explodable_node
      _(n).wont_be_nil
      _(n.left).must_equal(9)
      n.explode!
      _(t.to_a).must_equal([[[[0,9],2],3],4])
    end

    it "works for [7,[6,[5,[4,[3,2]]]]]" do
      t = Tree.treeify([7,[6,[5,[4,[3,2]]]]])
      n = t.explodable_node
      _(n.to_a).must_equal([3, 2])
      n.explode!
      _(t.to_a).must_equal([7,[6,[5,[7,0]]]])
    end

    it "works for [[6,[5,[4,[3,2]]]],1]" do
      t = Tree.treeify([[6,[5,[4,[3,2]]]],1])
      n = t.explodable_node
      _(n.to_a).must_equal([3, 2])
      n.explode!
      _(t.to_a).must_equal([[6,[5,[7,0]]],3])
    end

    it "works for [[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" do
      t = Tree.treeify([[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]])
      n = t.explodable_node
      _(n.to_a).must_equal([7, 3])
      n.explode!
      _(t.to_a).must_equal([[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]])
    end

    it "works for a step of complex example" do
      # this was the source of a serious headache for me.
      # the first [6,7] is explodable, the second [6,7] is where the right
      # literal should be (6), but it was going to 7,7.
      # It was walking up one more parent than it needs to before going right:
      # why?
      # It's because these two [6,7] nodes are the same value, *and* they're
      # also neighbors so they have the same parent, which based on my choice of
      # data modeling and Ruby's impl of `==` for structs makes them equal!
      # and the algorithm for finding the left/right neighbor uses equality of
      # nodes to determine the walk path. Oh, man, that kept me confused for way
      # too long. I thought it was the "how to walk a tree to the left/right
      # literal" that was messing me up, but no, my algorithm was logically
      # correct early on, it was just the semantics of == that messed me up.
      t = Tree.treeify([[[[[6, 7], [6, 7]], [[7, 7], [0, 7]]], [[[8, 7], [7, 7]], [[8, 8], [8, 0]]]], [[[[2, 4], 7], [6, [0, 5]]], [[[6, 8], [2, 8]], [[2, 1], [4, 5]]]]])
      n = t.explodable_node
      _(n.to_a).must_equal([6,7])
      _(n.left_literal_parent).must_be_nil
      _(n.right_literal_parent[0].to_a).must_equal([6,7]) # the other [6,7], not self
      _(n.right_literal_parent[1]).must_equal(:left)
    end
  end

  describe :split do
    it "works for [[[[0,7],4],[15,[0,13]]],[1,1]]" do
      t = Tree.treeify([[[[0,7],4],[15,[0,13]]],[1,1]])
      n = t.splittable_node
      _(n).wont_be_nil
      _(n.left).must_equal(15)
      n.split!
      _(t.to_a).must_equal([[[[0,7],4],[[7,8],[0,13]]],[1,1]])
    end
  end

  describe :reduce_once do
    it "reduces [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]] in stages" do
      t = Tree.treeify([[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]])
      t.reduce_once! # explode [4,3]
      _(t.to_a).must_equal([[[[0,7],4],[7,[[8,4],9]]],[1,1]])
      t.reduce_once! # explode [8,4]
      _(t.to_a).must_equal([[[[0,7],4],[15,[0,13]]],[1,1]])
      t.reduce_once! # split [15, ...]
      _(t.to_a).must_equal( [[[[0,7],4],[[7,8],[0,13]]],[1,1]])
      t.reduce_once! # split [..., 13]
      _(t.to_a).must_equal([[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]])
      t.reduce_once! # explode [6,7]
      _(t.to_a).must_equal([[[[0,7],4],[[7,8],[6,0]]],[8,1]])
    end
  end

  describe :reduce do
    it "reduces [[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]" do
      t = Tree.treeify([[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]])
      t.reduce!
      _(t.to_a).must_equal([[[[0,7],4],[[7,8],[6,0]]],[8,1]])
    end
  end

  describe :addition do
    it "reduces more complex example" do
      t = Tree.treeify([[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]])
      t += Tree.treeify([7,[[[3,7],[4,3]],[[6,3],[8,8]]]])
      _(t.to_a).must_equal([[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]])

      t += Tree.treeify([[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]])
      _(t.to_a).must_equal([[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]])

      t += Tree.treeify([[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]])
      _(t.to_a).must_equal([[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]])

      t += Tree.treeify([7,[5,[[3,8],[1,4]]]])
      _(t.to_a).must_equal([[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]])

      t += Tree.treeify([[2,[2,2]],[8,[8,1]]])
      _(t.to_a).must_equal([[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]])

      t += Tree.treeify([2,9])
      _(t.to_a).must_equal([[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]])

      t += Tree.treeify([1,[[[9,3],9],[[9,0],[0,7]]]])
      _(t.to_a).must_equal([[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]])

      t += Tree.treeify([[[5,[7,4]],7],1])
      _(t.to_a).must_equal([[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]])

      t += Tree.treeify([[[[4,2],2],6],[8,7]])
      _(t.to_a).must_equal([[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]])
    end
  end

  describe :magnitude do
    it "is correct for simple [9,1]" do
      n = Tree.treeify([9,1])
      _(n.magnitude).must_equal(29)
    end

    it "is correct for example sum" do
      n = Tree.treeify([[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]])
      _(n.magnitude).must_equal(4140)
    end
  end
end
