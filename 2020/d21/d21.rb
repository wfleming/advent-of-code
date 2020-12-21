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
end

list = FoodList.new(File.read(ARGV[0]))

# puts "DEBUG:
# p1 - count occurences of non allergenic ingredients
occurences = list.non_allergenic_ingredients.map do |ingredient|
  list.foods.count { |food| food.ingredients.include?(ingredient) }
end.sum
puts "p1: possible_allergen_ingredients=#{list.possible_allergen_ingredients}"
puts "p1: there are #{occurences} occurences of those ingredients"
