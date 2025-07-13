# Using map with symbol to proc
result1 = [1, 2, 3].map(&:to_s)
puts "Using map with &:to_s: #{result1}"

# Using map with block
result2 = [1, 2, 3].map { |n| n.to_s }
puts "Using map with block: #{result2}"

# Using for loop
result3 = []
for n in (1..3) do
  result3 << n.to_s
end
puts "Using for loop: #{result3}"
lambdas = [
  -> { "Hello" },
  -> { "World" },
  -> { "!" }
]

# Using while loop
a, result, i = [1, 2, 3], [], 0
while i < a.size do
  result << a[i].to_s
  i = i + 1
end
puts "Using while loop: #{result}"
(0...3).map { |n| -> { n } }
lambdas.each { |f| puts f.call }
