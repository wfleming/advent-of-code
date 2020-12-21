#!/usr/bin/env ruby

class Food
  attr_reader :ingredients, :allergens

  def self.parse(line)
    ingredients_str = /^([\w ]+)\(/.match(line)[1]
    ingredients = ingredients_str.split(" ").map(&:strip)
    allergens_str = /\(contains ([\w ,]+)\)/.match(line)[1]
    allergens = allergens_str.split(", ").map(&:strip)

    new(ingredients: ingredients, allergens: allergens)
  end

  def initialize(ingredients:, allergens:)
    @ingredients = ingredients
    @allergens = allergens
  end
end

class FoodList
  attr_reader :foods

  def initialize(str)
    @foods = str.lines.map { |l| Food.parse(l.strip) }
  end

  def all_allergens
    @all_allergens = foods.flat_map(&:allergens).uniq
  end

  def all_ingredients
    @all_ingredients = foods.flat_map(&:ingredients).uniq
  end

  # hash of allergen => [ingredients]
  def possible_allergen_ingredients
    @possible_allergen_ingredients = Hash[
      all_allergens.map { |allergen|
        candidates = foods.
          filter { |food| food.allergens.include?(allergen) }.
          map { |food| food.ingredients }.
          reduce(&:intersection)

        [allergen, candidates]
      }
    ]
  end

  def non_allergenic_ingredients
    @non_allergenic_ingredients = all_ingredients - possible_allergen_ingredients.values.flatten
  end

  # narrowed down to hash of allergen => ingredient
  def allergen_ingredients
    @allergen_ingredients ||=
      begin
        h = Hash[ possible_allergen_ingredients.map { |k, v| [k, v.clone] } ]

        while h.any? { |_, ingredients| ingredients.count > 1 }
          unique_ingredients = h.select { |_, ingredients|
            ingredients.count == 1
          }.values.flatten

          h = Hash[
            h.map do |allergen, ingredients|
              new_ingredients =
                if ingredients.count == 1
                  ingredients
                else
                  ingredients.reject { |i| unique_ingredients.include?(i) }
                end
              [allergen, new_ingredients]
            end
          ]
        end

        # now the ingredients are all 1-element arrays, unwrap them
        Hash[
          h.map { |allergen, ingredients| [allergen, ingredients[0]] }
        ]
      end
  end
end

list = FoodList.new(File.read(ARGV[0]))

# p1 - count occurences of non allergenic ingredients
occurences = list.non_allergenic_ingredients.map do |ingredient|
  list.foods.count { |food| food.ingredients.include?(ingredient) }
end.sum
puts "p1: there are #{occurences} occurences of those ingredients"

allergen_ingredients_alphabetical = list.allergen_ingredients.
  sort_by { |allergen, ingredient| allergen }.
  map { |pair| pair[1] }.
  join(",")
puts "p2: #{allergen_ingredients_alphabetical}"
