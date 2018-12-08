Node = Struct.new(:children, :metadata) do
  def each(&blk)
    yield self
    children.each { |c| c.each(&blk) }
  end

  def value
    if children.none?
      metadata.sum
    else
      metadata.map do |cid|
        children[cid - 1]
      end.compact.map(&:value).sum
    end
  end
end

class TreeBuilder
  def initialize(nums)
    @nums = nums
  end

  def tree
    next_node.tap do
      if @nums.any?
        raise ArgumentError, "parse had numbers left over, that seems wrong"
      end
    end
  end

  private

  def next_node
    child_count = @nums.shift
    metadata_count = @nums.shift

    children = child_count.times.map do
      next_node
    end

    metadata = @nums.shift(metadata_count)

    Node.new(children, metadata)
  end
end

def p1_checksum(tree)
  sum = 0

  tree.each do |n|
    sum += n.metadata.sum
  end

  sum
end

if $0 == __FILE__
  input = File.read(ARGV[0]).split.map(&:to_i)
  tree = TreeBuilder.new(input).tree

  puts "p1: metadata checksum is #{p1_checksum(tree)}"

  puts "p2: root node value is #{tree.value}"
end
