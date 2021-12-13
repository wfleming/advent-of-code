require "levenshtein"

class Matcher
  def initialize(list : Array(String))
    @list = list
  end

  def matched_ids
    @list.each do |str1|
      @list.each do |str2|
        if 1 == Levenshtein.distance(str1, str2)
          return {str1, str2}
        end
      end
    end
  end

  def shared_chars
     matches = matched_ids
     return nil if matches.nil?

     res = ""

     matches[0].each_char_with_index do |c1, idx|
       res = (res + c1) if matches[1][idx] == c1
     end

     res
  end
end
