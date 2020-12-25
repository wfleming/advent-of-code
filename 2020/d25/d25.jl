#!/usr/bin/env julia

function read_pub_keys()
  map(Base.Fix1(parse, Int64), eachline(ARGS[1]))
end

MODULUS = 20201227

function transform_once(subject, current_val)
  (current_val * subject) % MODULUS
end

function transform(subject, loops)
  val = subject

  # the first loop/stretch is really just "setting the value to the subject",
  # hence the subtraction
  for _ in 1:(loops - 1)
    val = transform_once(subject, val)
  end

  val
end

INITIAL_SUBJECT = 7

function find_loop_sizes(pub_key)
  loop_count = 1
  val = INITIAL_SUBJECT

  while val != pub_key
    loop_count += 1
    val = transform_once(INITIAL_SUBJECT, val)

    if loop_count > 100_000_000
      error("that loop count seems much too high")
    end
  end

  loop_count
end

pub_keys = read_pub_keys()

loop_sizes = map(find_loop_sizes, pub_keys)

enc_keys = map(
  (x) -> transform(x[1], x[2]),
  zip(pub_keys, reverse(loop_sizes))
)

if enc_keys[1] != enc_keys[2]
  error("both sides should get the same encryption key")
end

println("p1: encryption key is $(enc_keys[1])")
