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

const Passport = Dict{String,String}

FIELD_PAT = r"(\w+):([#\w]+)[ \n]"
PASS_SPLIT_PAT = r".*^$"m

function parse_passport(str):: Passport
  passport = Passport()

  # can't match end-of-string in a [] set with " ", and \n causing last field
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

# run the logic


# p1

input_str = read(open(ARGS[1]), String)
passports = parse_passports(input_str)
fields_except_cnt = filter(f -> f != "cid", REQUIRED_FIELDS)
valid_count = count(p -> is_passport_valid(fields_except_cnt, p), passports)
println("p1: $(valid_count) of $(length(passports)) passports are valid (sans `cid`)")
