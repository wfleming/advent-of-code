#!/usr/bin/env ruby

def flip_h(str)
  str.lines.map(&:chomp).map(&:reverse).join("\n")
end

def flip_v(str)
  str.lines.map(&:chomp).reverse.join("\n")
end

def rot_90(str)
  # firstrow becomes last col, and so on
  rows = str.lines.map(&:chomp)
  size = rows[0].length
  cols = (0...size).map { |c| rows.map { |r| r[c] }.join }
  cols.map(&:reverse).join("\n")
end

class PatternSet
  def self.parse(lines)
    new(lines.map { |l| l.strip.split(" => ") })
  end

  attr_reader :variations, :outputs

  def initialize(patterns)
    @outputs = {} # hash of canonical pattern => outputs
    @variations = {} # hash of variation => canonical

    patterns.each do |pat|
      tmpl, out = *pat
      tmpl = tmpl.gsub("/", "\n")
      out = out.gsub("/", "\n")
      @outputs[tmpl] = out
      @variations[tmpl] = tmpl
      @variations[flip_h(tmpl)] = tmpl
      @variations[flip_v(tmpl)] = tmpl
      3.times.inject(tmpl) do |t, _|
        t = rot_90(t)
        @variations[t] = tmpl
        @variations[flip_h(t)] = tmpl
        @variations[flip_v(t)] = tmpl
        t
      end
    end
  end

  def transform(chunk)
    outputs.fetch(variations.fetch(chunk))
  end
end

class Transformer
  def initialize(img, pat_set)
    @img = img
    @pat_set = pat_set
  end

  def img_size
    @img.lines.length
  end

  def chunk_size
    img_size % 2 == 0 ? 2 : 3
  end

  def chunks
    @img.lines.map(&:chomp).each_slice(chunk_size).flat_map do |lines|
      (0...img_size).each_slice(chunk_size).map { |idxs| lines.map { |l| l[idxs[0]..idxs[-1]] }.join("\n") }
    end
  end

  def transformed_chunks
    chunks.map(&@pat_set.method(:transform))
  end

  def output
    transformed_chunks.each_slice(img_size / chunk_size).flat_map do |line_chunks|
      (0..chunk_size).map { |row| line_chunks.map { |l| l.lines[row].chomp }.join }
    end.join("\n")
  end
end

if __FILE__ == $0
  seed = ".#.\n..#\n###"
  pattern_set = PatternSet.parse(File.readlines(ARGV[0]))

  iters = 5
  img = iters.times.inject(seed) { |img, _| Transformer.new(img, pattern_set).output }
  puts "p1: after #{iters} iterations, there are #{img.count("#")} pixels on"

  iters = 18
  img = iters.times.inject(seed) { |img, _| Transformer.new(img, pattern_set).output }
  puts "p2: after #{iters} iterations, there are #{img.count("#")} pixels on"
end
