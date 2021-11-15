#!/usr/bin/env ruby

def parse_input
  lines = File.readlines(ARGV[0])

  transforms = lines.inject({}) do |memo, l|
    if (m = /^(\w+) => (\w+)/.match(l))
      memo[m[1]] ||= []
      memo[m[1]] << m[2]
    end
    memo
  end

  # transforms is hash of { source => [possible transforms] }
  return [lines[-1].chomp, transforms]
end

def available_transforms(molecule, transforms)
  transforms.flat_map do |source, replacements|
    new_molecules = []
    search_offset = 0
    while (i = molecule.index(source, search_offset))
      new_molecules += replacements.map do |replacement|
        left = molecule.chars.take(i).join("")
        right = molecule.chars.drop(i + source.length).join("")
        "#{left}#{replacement}#{right}"
      end
      search_offset = i + 1
    end
    new_molecules
  end.uniq
end

medicine_molecule, transforms = *parse_input

potential_next_molecules = available_transforms(medicine_molecule, transforms)
puts "p1: #{potential_next_molecules.count} potential molecules can be created with 1 transform"
