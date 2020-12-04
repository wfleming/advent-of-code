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
  byr = parse(Int64, passport["byr"])
  if !(byr >= 1920 && byr <= 2002)
    return false
  end

  iyr = parse(Int64, passport["iyr"])
  if !(iyr >= 2010 && iyr <= 2020)
    return false
  end

  eyr = parse(Int64, passport["eyr"])
  if !(eyr >= 2020 && eyr <= 2030)
    return false
  end

  hgt_parts = match(r"(\d+)(cm|in)", passport["hgt"])
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

  if nothing == match(HAIR_PAT, passport["hcl"])
    return false
  end

  if !(passport["ecl"] in EYE_COLORS)
    return false
  end

  if nothing == match(PID_PAT, passport["pid"])
    return false
  end

  true
end

# run the logic

# p1

input_str = read(open(ARGS[1]), String)
passports = parse_passports(input_str)
fields_except_cnt = filter(f -> f != "cid", REQUIRED_FIELDS)
valid_count = count(p -> is_passport_valid(fields_except_cnt, p), passports)
println("p1: $(valid_count) of $(length(passports)) passports are valid (sans `cid`)")

valid_count_p2 = count(p -> is_passport_valid_p2(fields_except_cnt, p), passports)
println("p2: $(valid_count_p2) of $(length(passports)) passports are valid (sans `cid`)")
