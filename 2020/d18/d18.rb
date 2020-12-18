#!/usr/bin/env ruby

module Calculator
  TOKENS = %r{(\d+|\(|\)|\+|\-|\*|/)}

  def self.tokenize(str)
    # parsing numbers isn't strictly tokenizing but this is convenient
    str.strip.scan(TOKENS).to_a.map do |tok|
      tok = tok[0] # scan puts each match in its own array
      if /\d+/ =~ tok
        Integer(tok)
      else
        tok.to_sym
      end
    end
  end

  # group by parens
  def self.lex(tokens, level = 0)
    # puts "#{"  " * level}DEBUG: lex #{tokens}"
    exprs = []
    while tokens.any?
      t = tokens.shift
      if t == :"(" # start of sub-expr
        exprs << lex(tokens, level + 1)
      elsif t == :")" # end of sub-expr
        # puts "#{"  " * level}DEBUG: lex end sub-expr #{exprs}, tokens=#{tokens}"
        raise "mismatched parens" if level == 0 && tokens.any?
        return exprs # early return a sub-expr
      else # just a regular token
        exprs << t
      end
    end

    exprs
  end

  def self.evaluate(expr)
    # puts "DEBUG evaluate: #{expr}"
    case expr
    when Numeric
      expr
    when Array
      v = evaluate(expr.shift)
      expr.each_slice(2).reduce(v) { |ans, op_rhs|
        calc_op(ans, op_rhs[0], op_rhs[1])
      }
    end
  end

  def self.calc_op(lhs, op, rhs)
    case op
    when :+
      evaluate(lhs) + evaluate(rhs)
    when :-
      evaluate(lhs) - evaluate(rhs)
    when :*
      evaluate(lhs) * evaluate(rhs)
    when :/
      evaluate(lhs) / evaluate(rhs)
    else
      raise "unexpected op #{op} with lhs #{lhs} and rhs #{rhs}"
    end
  end
end

# p1
exprs = File.readlines(ARGV[0]).map { |l| Calculator.lex(Calculator.tokenize(l)) }
vals = exprs.map { |e| Calculator.evaluate(e) }
puts "p1: sum of all evaluated expressions is #{vals.sum}"
