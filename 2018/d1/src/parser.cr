class Parser
  def parse(input)
    input.lines.map do |line|
      line.to_i
    end
  end
end
