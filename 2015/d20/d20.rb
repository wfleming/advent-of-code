#!/usr/bin/env ruby

def smallest_prime_divisor(n)
  p = 2
  while n >= p**2
    if n % p == 0 then
      return p
      n = n / p
    else
      p = p + 1
    end
  end
  n
end

FACTORS_LOOKUP = Hash.new do |hsh, n|
  if n == 1
    hsh[n] = [1]
  else
    p = smallest_prime_divisor(n)
    if p == n
      hsh[n] = [1, n]
    else
      fs = (hsh[p] + hsh[n / p] + [n]).uniq
      hsh[n] = (fs + fs.map { |f| n / f }).uniq.sort
    end
  end
end

def house_gifts(n)
  FACTORS_LOOKUP[n].sum * 10
end

def house_gifts_p2(n)
  FACTORS_LOOKUP[n].select { |f| n <= f * 50 }.sum * 11
end

TARGET_MIN = Integer(ARGV[0])
p1_ans = 1
p1_ans += 1 while house_gifts(p1_ans) < TARGET_MIN

puts "p1: house #{p1_ans} is first to get #{TARGET_MIN} gifts (it gets #{house_gifts(p1_ans)} gifts)"

p2_ans = 1
p2_ans += 1 while house_gifts_p2(p2_ans) < TARGET_MIN

puts "p2: house #{p2_ans} is first to get #{TARGET_MIN} gifts (it gets #{house_gifts_p2(p2_ans)} gifts)"
