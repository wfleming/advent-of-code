#!/usr/bin/env julia

REQUIRED_FIELDS = [
  "byr",
  "iyr",
  "eyr",
  "hgt",
  "hcl",
  "ecl",
  "pid",
  "cid",
]

EYE_COLORS = [
  "amb",
  "blu",
  "brn",
  "gry",
  "grn",
  "hzl",
  "oth",
]

const Passport = Dict{String,String}

FIELD_PAT = r"(\w+):([#\w]+)[ \n]"
PASS_SPLIT_PAT = r".*^$"m
HAIR_PAT = r"^#[\da-f]{6}$"
PID_PAT = r"^\d{9}$"

function parse_passport(str):: Passport
  passport = Passport()

  # can't match end-of-string (i.e. \Z) in a [] set causing last field
  # to get dropped. Append a field sep before matching
  str = str * " "

  for m in eachmatch(FIELD_PAT, str)
    passport[m[1]] = m[2]
  end

  passport
end

function parse_passports(str):: Array{Passport}
  map(
    s -> parse_passport(strip(s)),
    split(str, PASS_SPLIT_PAT)
  )
end

function is_passport_valid(fields, passport):: Bool
  ks = keys(passport)
  all(f -> f in ks, fields)
end

function is_passport_valid_p2(fields, passport):: Bool
  if !is_passport_valid(fields, passport)
    return false
  end

  # now we know the fields are present, we can check the vals
  validations = Dict(
    "byr" => byr -> begin
      byr = parse(Int64, byr)
      byr >= 1920 && byr <= 2002
    end,
    "iyr" => iyr -> begin
      iyr = parse(Int64, iyr)
      iyr >= 2010 && iyr <= 2020
    end,
    "eyr" => eyr -> begin
      eyr = parse(Int64, eyr)
      eyr >= 2020 && eyr <= 2030
    end,
    "hgt" => hgt -> begin
      hgt_parts = match(r"(\d+)(cm|in)", hgt)
      if nothing == hgt_parts
        return false
      end
      hgt_n = parse(Int64, hgt_parts[1])
      if hgt_parts[2] == "cm"
        if !(hgt_n >= 150 && hgt_n <= 193)
          return false
        end
      elseif hgt_parts[2] == "in"
        if !(hgt_n >= 59 && hgt_n <= 76)
          return false
        end
      else
        return false
      end

      true
    end,
    "hcl" => hcl -> nothing != match(HAIR_PAT, hcl),
    "ecl" => ecl -> ecl in EYE_COLORS,
    "pid" => pid -> nothing != match(PID_PAT, pid),
  )

  all(
    attr -> validations[attr](passport[attr]),
    keys(validations)
  )
end

# run the logic

input_str = read(open(ARGS[1]), String)
passports = parse_passports(input_str)

# p1

fields_except_cnt = filter(f -> f != "cid", REQUIRED_FIELDS)
valid_count = count(p -> is_passport_valid(fields_except_cnt, p), passports)
println("p1: $(valid_count) of $(length(passports)) passports are valid (sans `cid`)")

# p2

valid_count_p2 = count(p -> is_passport_valid_p2(fields_except_cnt, p), passports)
println("p2: $(valid_count_p2) of $(length(passports)) passports are valid (sans `cid`)")
