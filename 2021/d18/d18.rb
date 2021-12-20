#!/usr/bin/env ruby

require "json"

Tree = Struct.new(:parent, :left, :right) do
  def self.treeify(sn, cur_parent = nil)
    raise "invariant violation" if sn.count != 2
    cur_node = Tree.new(cur_parent, nil, nil)
    cur_node.left = sn[0].is_a?(Numeric) ? sn[0] : treeify(sn[0], cur_node)
    cur_node.right = sn[1].is_a?(Numeric) ? sn[1] : treeify(sn[1], cur_node)
    cur_node
  end

  def ==(other)
    __id__ == other.__id__
  end

  def to_a
    left_val = left.is_a?(Numeric) ? left : left.to_a
    right_val = right.is_a?(Numeric) ? right : right.to_a
    [left_val, right_val]
  end

  def root
    parent.nil? ? self : parent.root
  end

  def depth
    if parent.nil? # this is root
      0
    else
      1 + parent.depth
    end
  end

  def deep_clone(new_parent = nil)
    new_parent = self.class.new(new_parent, nil, nil)
    new_parent.left = left.is_a?(Numeric) ? left : left.deep_clone(new_parent)
    new_parent.right = right.is_a?(Numeric) ? right : right.deep_clone(new_parent)
    new_parent
  end

  def +(other)
    raise "can't add non-root trees" if self.parent || other.parent
    Tree.new(nil, deep_clone, other.deep_clone).tap do |new_root|
      new_root.left.parent = new_root
      new_root.right.parent = new_root
      new_root.reduce!
    end
  end

  def each(&block)
    left.each(&block) if left.is_a?(self.class)
    yield self
    right.each(&block) if right.is_a?(self.class)
  end

  def magnitude
    lm = left.is_a?(Numeric) ? left : left.magnitude
    rm = right.is_a?(Numeric) ? right : right.magnitude
    (3 * lm) + (2 * rm)
  end

  def explodable_node
    self.each do |n|
      return n if n.depth >= 4 && n.left.is_a?(Numeric) && n.right.is_a?(Numeric)
    end
    nil
  end

  def splittable_node
    self.each do |n|
      return n if (n.left.is_a?(Numeric) && n.left >= 10) || (n.right.is_a?(Numeric) && n.right >= 10)
    end
    nil
  end

  def left_literal_parent
    n = self
    while n&.parent&.left == n
      n = n.parent
    end
    n = n&.parent
    return nil if n.nil?
    return [n, :left] if n.left.is_a?(Numeric)
    n = n.left
    n = n.right until n.right.is_a?(Numeric)
    [n, :right]
  end

  def right_literal_parent
    n = self
    while n&.parent&.right == n
      n = n.parent
    end
    n = n&.parent
    return nil if n.nil?
    return [n, :right] if n.right.is_a?(Numeric)
    n = n.right
    n = n.left until n.left.is_a?(Numeric)
    [n, :left]
  end

  # def add_left_from!(node)
  #   if node == right
  #     if left.is_a?(Numeric)
  #       left += node.left
  #     else
  #       left.add_right_from!(node)
  #     end
  #   else

  #   end
  # end

  # def add_right_from!(node)
  # end

  def explode!
    left_neighbor = left_literal_parent
    if left_neighbor
      node, attr = *left_neighbor
      node.send("#{attr}=", node.send(attr) + left)
    end
    right_neighbor = right_literal_parent
    if right_neighbor
      node, attr = *right_neighbor
      node.send("#{attr}=", node.send(attr) + right)
    end

    # replace self with 0
    parent.left == self ? parent.left = 0 : parent.right = 0
  end

  def split!
    if left.is_a?(Numeric) && left >= 10
      self.left = Tree.new(self, left / 2, (left.to_f / 2).ceil)
    elsif right.is_a?(Numeric) && right >= 10
      self.right = Tree.new(self, right / 2, (right.to_f / 2).ceil)
    else
      raise "trying to split a node without a splittable value"
    end
  end

  def reduce_once!
    if (n = explodable_node)
      n.explode!
      return true
    elsif (n = splittable_node)
      n.split!
      return true
    end
    false
  end

  def reduce!
    reducable = true
    while reducable
      reducable = reduce_once!
    end
  end
end

if __FILE__ == $0
  snail_nums = File.readlines(ARGV[0]).map(&JSON.method(:parse)).map(&Tree.method(:treeify))
  sum = snail_nums.reduce(&:+)
  puts "p1: reduced sum #{sum.to_a} has magnitude #{sum.magnitude}"

  additions = Hash[
    snail_nums.permutation(2).map { |a,b| [[a,b], a + b] }
  ]
  winner = additions.max_by { |_, v| v.magnitude }
  addend0, addend1 = winner[0]
  sum = winner[1]
  puts "p2: #{addend0.to_a} + #{addend1.to_a} = #{sum.to_a}, magnitude = #{sum.magnitude}"
end
