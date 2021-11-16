require "set"

# https://www.brianstorti.com/implementing-a-priority-queue-in-ruby/
class PQueue
  def initialize()
    # each element is [actual_element, priority], root remains nil
    @storage = [nil]
    @includes = Set.new
  end

  def any?
    count > 0
  end

  def count
    # don't count nil root
    @storage.count - 1
  end

  def include?(x)
    @includes.include?(x)
  end

  # only returns the element, not the stored priority
  def shift
    swap(1, @storage.size - 1)
    head = @storage.pop # head/highest priority is now at end of storage
    bubble_down(1) # make sure the tree is ordered again
    @includes.delete(head[0])
    head[0]
  end

  # Priority is inverted - lower numbers are "higher" priority
  def push(el, priority)
    @storage << [el, priority]
    @includes << el
    bubble_up(@storage.count - 1)
  end

  def bubble_up(idx)
    parent_idx = idx / 2
    return if parent_idx < 1
    return if @storage[parent_idx][1] <= @storage[idx][1]
    swap(idx, parent_idx)
    bubble_up(parent_idx)
  end

  def bubble_down(idx)
    child_index = idx * 2
    return if child_index > @storage.count - 1
    not_last_el = child_index < @storage.count - 1
    left_child = @storage[child_index]
    right_child = @storage[child_index + 1]
    child_index += 1 if not_last_el && right_child[1] < left_child[1]
    return if @storage[idx][1] <= @storage[child_index][1]
    swap(idx, child_index)
    bubble_down(child_index)
  end

  def swap(idx1, idx2)
    @storage[idx1], @storage[idx2] = @storage[idx2], @storage[idx1]
  end
end
