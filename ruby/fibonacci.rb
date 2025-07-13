#!/usr/bin/env ruby

def fibonacci(n)
  return n if n <= 1
  a, b = 0, 1
  (2..n).each { a, b = b, a + b }
  b
end

if ARGV.empty?
  puts "Usage: #{File.basename($0)} <number>"
  exit 1
end

n = ARGV[0].to_i
puts "Fibonacci(#{n}) = #{fibonacci(n)}"
