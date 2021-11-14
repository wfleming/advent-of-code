#!/usr/bin/env ruby

Ingredient = Struct.new(:name, :capacity, :durability, :flavor, :texture, :calories) do
  PAT = /(?<name>\w+): capacity (?<capacity>-?\d+), durability (?<durability>-?\d+), flavor (?<flavor>-?\d+), texture (?<texture>-?\d+), calories (?<calories>\d+)/

  def self.parse(line)
    m = PAT.match(line)
    return self.new(m["name"], m["capacity"].to_i, m["durability"].to_i, m["flavor"].to_i, m["texture"].to_i, m["calories"].to_i)
  end
end

class Recipe
  TOTAL_TSP = 100
  P1_SCORE_ATTRS = [:capacity, :durability, :flavor, :texture]

  def initialize(ingredients)
    # ingredients is hash of Ingredient => tsp amount
    @ingredients = ingredients
    if ingredients.values.sum != TOTAL_TSP
      raise ArgumentError, "Ingredients don't add up to 100: #{ingredients.inspect}"
    end
  end

  def score
    @score ||= P1_SCORE_ATTRS.map do |attr|
      t = @ingredients.map { |i, a| i.send(attr) * a }.sum
      t = [t, 0].max
    end.inject(1, &:*)
  end

  def calories
    @calories ||= @ingredients.map do |ingredient, amt|
      ingredient.calories * amt
    end.sum
  end
end

def generate_recipes(remaining_ingredients, cur_ingredients=nil, final_count=nil)
  if cur_ingredients.nil? # first call, start seeding recipes
    first_ingredient, other_ingredients = remaining_ingredients.first, remaining_ingredients.drop(1)
    (1..(Recipe::TOTAL_TSP - other_ingredients.count)).flat_map do |first_amt|
      generate_recipes(other_ingredients, {first_ingredient => first_amt}, remaining_ingredients.count)
    end.map(&Recipe.method(:new))
  else # fill the next ingredient for the current recipe, then recurse
    next_ingredient, other_ingredients = remaining_ingredients.first, remaining_ingredients.drop(1)
    if other_ingredients.empty? # last ingredient, so its quantity will be fixed
      next_amt = Recipe::TOTAL_TSP - cur_ingredients.values.sum
      recipe_ingredients = cur_ingredients.merge(next_ingredient => next_amt)
      if recipe_ingredients.count != final_count
        raise ArgumentError, "Creating a recipe with #{recipe_ingredients.count} ingredients, but there should be #{final_count}"
      end
      recipe_ingredients
    else
      next_max = Recipe::TOTAL_TSP - cur_ingredients.values.sum - other_ingredients.count
      (1..next_max).flat_map do |next_amt|
        generate_recipes(other_ingredients, cur_ingredients.merge(next_ingredient => next_amt), final_count)
      end
    end
  end
end

ingredients = File.readlines(ARGV[0]).map(&Ingredient.method(:parse))

all_recipes = generate_recipes(ingredients).sort_by(&:score)

puts "p1: best recipe has score of #{all_recipes[-1].score}"

p2_recipes = all_recipes.select { |r| r.calories == 500 }
puts "p1: best recipe has score of #{p2_recipes[-1].score}"
