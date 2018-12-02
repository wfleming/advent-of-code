class Checksum
  def initialize(list : Array(String))
    @list = list
  end

  def checksum
    two_repeat_count = 0
    three_repeat_count = 0

    tuples.each do |tuple|
      two_repeat_count += 1 if tuple[0]
      three_repeat_count += 1 if tuple[1]
    end

    two_repeat_count * three_repeat_count
  end

  private def tuples
    @list.map do |string|
      string_checksum_tuple(string)
    end
  end

  private def string_checksum_tuple(string)
    two_repeated = false
    three_repeated = false
    string.each_char do |char|
      two_repeated = true if 2 == char_count(char, string)
      three_repeated = true if 3 == char_count(char, string)
    end

    return {two_repeated, three_repeated}
  end

  private def char_count(char, string)
    matches = string.each_char.select do |target_char|
      target_char == char
    end.size
  end
end
